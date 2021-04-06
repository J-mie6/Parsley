package parsley.internal.deepembedding

import scala.language.{higherKinds, implicitConversions}
import scala.annotation.tailrec
import scala.collection.mutable

import parsley.BadLazinessException
import parsley.registers.Reg
import parsley.internal.machine.instructions, instructions.{Instr, JumpTable, Label}
import parsley.internal.ResizableArray
import Parsley.allocateRegisters
import ContOps.{safeCall, GenOps, perform, result, ContAdapter}

/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `runParser` will
  * parse the string given as input to `runParser`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the class itself is abstract
  *
  * @author Jamie Willis
  * @version 1
  */
private [parsley] abstract class Parsley[+A] private [deepembedding]
{
    final protected type InstrBuffer = ResizableArray[Instr]
    final protected type T = Any
    final protected type U = Any
    final protected type V = Any

    // $COVERAGE-OFF$
    final private [parsley] def prettyAST: String = {force(); safeCall((g: GenOps[String]) => perform(prettyASTAux(g))(g))}
    // $COVERAGE-ON$

    final def unsafe(): Unit = safe = false
     // $COVERAGE-OFF$
    final def force(): Unit = instrs
    final def overflows(): Unit = cps = true
     // $COVERAGE-ON$
    private [deepembedding] def demandCalleeSave(): this.type = {
        calleeSaveNeeded = true
        this
    }

    // Internals
    final private [deepembedding] def findLets[Cont[_, +_], R]()(implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = {
        state.addPred(this)
        if (seen(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) {
            this match {
                case self: UsesRegister => state.addReg(self.reg)
                case _ =>
            }

            try findLetsAux(ops, seen + this, state)
            catch {
                case npe: NullPointerException => throw new BadLazinessException
            }
        }
        else result(())
    }
    final private def applyLets[Cont[_, +_], R](implicit seen: Set[Parsley[_]], sub: SubMap, recs: RecMap[Cont]): Parsley[A] = {
        // We use the seen set here to prevent cascading sub-routines
        val wasSeen = seen(this)
        val self = sub(this)
        if (wasSeen && (self eq this)) recs(this)
        else if (wasSeen) this
        else self
    }
    final private [deepembedding] def optimised[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]],
                                                                                        sub: SubMap, recs: RecMap[Cont]): Cont[R, Parsley[A_]] = {
        val fixed = this.applyLets
        val _seen = seen // Not needed in Scala 3, but really?!
        if (fixed.processed) result(fixed.optimise)
        else {
            implicit val seen: Set[Parsley[_]] = _seen + this
            for (p <- fixed.preprocess) yield p.optimise
        }
    }
    final private [deepembedding] var safe = true
    final private var cps = false
    final private [deepembedding] var size: Int = 1
    final private [deepembedding] var processed = false
    final private var calleeSaveNeeded = false

    final private def generateCalleeSave[Cont[_, +_], R](bodyGen: =>Cont[R, Unit], allocatedRegs: List[Int])
                                                        (implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        if (calleeSaveNeeded && allocatedRegs.nonEmpty) {
            val end = state.freshLabel()
            val calleeSave = state.freshLabel()
            instrs += new instructions.Label(calleeSave)
            instrs += new instructions.CalleeSave(end, allocatedRegs)
            bodyGen |> {
                instrs += new instructions.Jump(calleeSave)
                instrs += new instructions.Label(end)
            }
        }
        else bodyGen
    }

    final private def pipeline[Cont[_, +_]](implicit ops: ContOps[Cont, Unit]): Array[Instr] ={
        implicit val instrs: InstrBuffer = new ResizableArray()
        implicit val state: CodeGenState = new CodeGenState
        implicit val letFinderState: LetFinderState = new LetFinderState
        implicit lazy val subMap: SubMap = new SubMap(letFinderState.lets)
        implicit lazy val recMap: RecMap[Cont] = new RecMap(subMap, letFinderState.recs, state)
        perform {
            implicit val seenSet: Set[Parsley[_]] = Set.empty
            findLets() >> {
                implicit val seenSet: Set[Parsley[_]] = letFinderState.recs
                implicit val usedRegs: Set[Reg[_]] = letFinderState.usedRegs
                optimised.flatMap(p => generateCalleeSave(p.codeGen, allocateRegisters(usedRegs))) |> {
                    val end = generatePreamble(state.subsExist || seenSet.nonEmpty)
                    finaliseRecs()
                    finaliseSubs()
                    generatePostamble(end)
                }
            }
        }
        finaliseInstrs(instrs, state, recMap.iterator)
    }

    final private def generatePreamble(required: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): Option[Int] = if (!required) None else {
        val end = state.freshLabel()
        instrs += new instructions.Jump(end)
        Some(end)
    }

    final private def generatePostamble(endLabel: Option[Int])(implicit instrs: InstrBuffer): Unit = {
        for (end <- endLabel) instrs += new instructions.Label(end)
    }

    final private def finaliseRecs[Cont[_, +_]]()(implicit ops: ContOps[Cont, Unit], instrs: InstrBuffer, state: CodeGenState, lets: SubMap, recs: RecMap[Cont]): Unit = {
        implicit val seenSet: Set[Parsley[_]] = Set.empty
        for (rec <- recs) {
            instrs += new instructions.Label(rec.label)
            val start = instrs.length
            perform(rec.p.optimised.flatMap(_.codeGen))
            instrs += instructions.Return
        }
    }

    final private [deepembedding] def computeRecInstrs[Cont[_, +_]](subs: SubMap, recs: RecMap[Cont])(implicit ops: ContOps[Cont, Unit]): Array[Instr] = {
        implicit val seenSet: Set[Parsley[_]] = Set.empty
        implicit val subMap: SubMap = subs
        implicit val recMap: RecMap[Cont] = recs
        implicit val instrs: InstrBuffer = new ResizableArray()
        implicit val state: CodeGenState = new CodeGenState
        perform(optimised.flatMap(_.codeGen))
        val endLabel = generatePreamble(state.subsExist)
        finaliseSubs()
        generatePostamble(endLabel)
        finaliseInstrs(instrs, state, Iterator.empty)
    }

    final private def finaliseSubs[Cont[_, +_]]()(implicit ops: ContOps[Cont, Unit], instrs: InstrBuffer, state: CodeGenState): Unit = {
        while (state.more) {
            val sub = state.nextSub()
            instrs += new instructions.Label(state.getLabel(sub))
            perform(sub.p.codeGen)
            instrs += instructions.Return
        }
    }

    final private def computeInstrs(ops: GenOps[Unit]): Array[Instr] = pipeline(ops)

    final private def finaliseInstrs(instrs: InstrBuffer, state: CodeGenState, recs: Iterator[Rec[_]]): Array[Instr] = {
        @tailrec def findLabels(instrs: Array[Instr], labels: Array[Int], n: Int, i: Int, off: Int): Int = if (i + off < n) instrs(i + off) match {
            case label: Label =>
                instrs(i + off) = null
                labels(label.i) = i
                findLabels(instrs, labels, n, i, off + 1)
            case _ => findLabels(instrs, labels, n, i + 1, off)
        }
        else i
        @tailrec def applyLabels(srcs: Array[Instr], labels: Array[Int], dests: Array[Instr], n: Int, i: Int, off: Int): Unit = if (i < n) srcs(i + off) match {
            case null => applyLabels(srcs, labels, dests, n, i, off + 1)
            case instr =>
                dests(i) = instr.relabel(labels)
                applyLabels(srcs, labels, dests, n, i + 1, off)
        }
        val instrsOversize = instrs.toArray
        val labelMapping = new Array[Int](state.nlabels)
        val size = findLabels(instrsOversize, labelMapping, instrs.length, 0, 0)
        val instrs_ = new Array[Instr](size)
        applyLabels(instrsOversize, labelMapping, instrs_, instrs_.length, 0, 0)
        for (rec <- recs) {
            rec.statefulIndices = instructions.statefulIndicesToReturn(instrs_, rec.label)
            //if (rec.statefulIndices.nonEmpty) println(rec.statefulIndices.map(instrs_).mkString(", "))
        }
        instrs_
    }

    final private [parsley] lazy val instrs: Array[Instr] = if (cps) computeInstrs(Cont.ops.asInstanceOf[GenOps[Unit]]) else safeCall(computeInstrs(_))
    final private lazy val pindices: Array[Int] = instructions.statefulIndices(instrs)
    final private [parsley] def threadSafeInstrs: Array[Instr] = instructions.stateSafeCopy(instrs, pindices)

    // This is a trick to get tail-calls to fire even in the presence of a legimate recursion
    final private [deepembedding] def optimiseDefinitelyNotTailRec: Parsley[A] = optimise

    // Abstracts
    // Sub-tree optimisation and Rec calculation - Bottom-up
    protected def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], sub: SubMap, recs: RecMap[Cont]): Cont[R, Parsley[A_]]
    // Let-finder recursion
    protected def findLetsAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit]
    // Optimisation - Bottom-up
    protected def optimise: Parsley[A] = this
    // Peephole optimisation and code generation - Top-down
    private [parsley] def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit]
    private [parsley] def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String]
}
private [deepembedding] object Parsley {
    private def applyAllocation(regs: Set[Reg[_]], freeSlots: Iterable[Int]): List[Int] = {
        val allocatedSlots = mutable.ListBuffer.empty[Int]
        for ((reg, addr) <- regs.zip(freeSlots)) {
            reg.allocate(addr)
            allocatedSlots += addr
        }
        allocatedSlots.toList
    }

    private [Parsley] def allocateRegisters(regs: Set[Reg[_]]): List[Int] = {
        // Global registers cannot occupy the same slot as another global register
        // In a flatMap, that means a newly discovered global register must be allocated to a new slot
        // This should resize the register pool, but under current restrictions we'll just throw an
        // excepton if there are no available slots
        val unallocatedRegs = regs.filterNot(_.allocated)
        if (unallocatedRegs.nonEmpty) {
            val usedSlots = regs.collect {
                case reg if reg.allocated => reg.addr
            }
            val freeSlots = (0 until 4).filterNot(usedSlots)
            if (unallocatedRegs.size > freeSlots.size) {
                throw new IllegalStateException("Current restrictions require that the maximum number of registers in use is 4")
            }
            applyAllocation(unallocatedRegs, freeSlots)
        }
        else Nil
    }
}

private [deepembedding] trait MZero extends Parsley[Nothing]
private [deepembedding] trait UsesRegister {
    val reg: Reg[_]
}

// Internals
private [deepembedding] class CodeGenState {
    private var current = 0
    private val queue = mutable.ListBuffer.empty[Subroutine[_]]
    private val map = mutable.Map.empty[Subroutine[_], Int]
    def freshLabel(): Int = {
        val next = current
        current += 1
        next
    }
    def nlabels: Int = current

    def getLabel(sub: Subroutine[_]): Int = map.getOrElseUpdate(sub, {
        sub +=: queue
        freshLabel()
    })

    def nextSub(): Subroutine[_] = queue.remove(0)
    def more: Boolean = queue.nonEmpty
    def subsExist: Boolean = map.nonEmpty
}

private [deepembedding] class LetFinderState {
    private val _recs = mutable.Set.empty[Parsley[_]]
    private val _preds = mutable.Map.empty[Parsley[_], Int]
    private val _usedRegs = mutable.Set.empty[Reg[_]]

    def addPred(p: Parsley[_]): Unit = _preds += p -> (_preds.getOrElseUpdate(p, 0) + 1)
    def addRec(p: Parsley[_]): Unit = _recs += p
    def addReg(reg: Reg[_]): Unit = _usedRegs += reg
    def notProcessedBefore(p: Parsley[_]): Boolean = _preds(p) == 1

    def lets: Iterable[Parsley[_]] = _preds.toSeq.view.collect {
        case (p, refs) if refs >= 2 && !_recs(p) => p
    }
    lazy val recs: Set[Parsley[_]] = _recs.toSet
    def usedRegs: Set[Reg[_]] = _usedRegs.toSet
}

private [deepembedding] abstract class ParserMap[V <: Parsley[_]](ks: Iterable[Parsley[_]]) {
    protected def make(p: Parsley[_]): V
    protected val map: Map[Parsley[_], V] = ks.map {
        case p => p -> make(p)
    }.toMap
    override def toString: String = map.toString
}

private [deepembedding] class SubMap(subs: Iterable[Parsley[_]]) extends ParserMap[Subroutine[_]](subs) {
    def make(p: Parsley[_]) = new Subroutine(p)
    def apply[A](p: Parsley[A]): Parsley[A] = map.getOrElse(p, p).asInstanceOf[Parsley[A]]
}

private [deepembedding] class RecMap[Cont[_, +_]](subs: SubMap, recs: Iterable[Parsley[_]], state: CodeGenState)(implicit ops: ContOps[Cont, Unit])
    extends ParserMap[Rec[_]](recs) with Iterable[Rec[_]] {
    def make(p: Parsley[_]) = new Rec(p, new instructions.Call(p.computeRecInstrs(subs, this), state.freshLabel()))
    def apply[A](p: Parsley[A]): Rec[A] = map(p).asInstanceOf[Rec[A]]
    override def iterator: Iterator[Rec[_]] = map.values.iterator
}