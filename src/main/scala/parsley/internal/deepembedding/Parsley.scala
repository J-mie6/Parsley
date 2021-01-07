package parsley.internal.deepembedding

import scala.language.{higherKinds, implicitConversions}
import scala.annotation.tailrec
import scala.collection.mutable

import parsley.Reg
import parsley.internal.instructions, instructions.{Instr, JumpTable, JumpInstr, Label}
import parsley.internal.{UnsafeOption, ResizableArray}
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
    final private [parsley] def prettyAST: String = {force(); safeCall((g: GenOps) => perform(prettyASTAux(g))(g))}
    // $COVERAGE-ON$

    final def unsafe(): Unit = safe = false
    final def force(): Unit = instrs
    final def overflows(): Unit = cps = true

    // Internals
    final private [deepembedding] def findLets[Cont[_, +_]: ContOps](implicit seen: Set[Parsley[_]], state: LetFinderState): Cont[Unit, Unit] = {
        state.addPred(this)
        if (seen(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) {
            this match {
                case self: UsesRegister => state.addReg(self.reg)
                case _ =>
            }
            findLetsAux(implicitly[ContOps[Cont]], seen + this, state)
        }
        else result(())
    }
    final private def fix(implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String]): Parsley[A] = {
        // We use the seen set here to prevent cascading sub-routines
        val wasSeen = seen(this)
        val self = sub(this)
        if (wasSeen && (self eq this)) new Rec(this, label)
        else if (wasSeen) this
        else self
    }
    final private [deepembedding] def optimised[Cont[_, +_]: ContOps, A_ >: A](implicit seen: Set[Parsley[_]],
                                                                                        sub: SubMap,
                                                                                        label: UnsafeOption[String] = null): Cont[Unit, Parsley[A_]] = {
        for (p <- this.fix.preprocess(implicitly[ContOps[Cont]], seen + this, sub, label)) yield p.optimise
    }
    final private [deepembedding] var safe = true
    final private var cps = false
    final private [deepembedding] var size: Int = 1
    final private [deepembedding] var processed = false

    final private def pipeline[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Unit = {
        perform {
            implicit val letFinderState: LetFinderState = new LetFinderState
            implicit val seenSet: Set[Parsley[_]] = Set.empty
            findLets >> {
                implicit val subMap: SubMap = new SubMap(letFinderState.lets)
                optimised.flatMap(p => {
                    allocateRegisters(letFinderState.usedRegs)
                    p.codeGen
                })
            }
        }
        if (state.map.nonEmpty) {
            val end = state.freshLabel()
            instrs += new instructions.Jump(end)
            val map = state.map
            while (state.more) {
                val p = state.nextSub()
                val label = map(p)
                instrs += new instructions.Label(label)
                perform(p.codeGen)
                instrs += instructions.Return
            }
            instrs += new instructions.Label(end)
        }
    }

    final private def computeInstrs(ops: GenOps): Array[Instr] = {
        val instrs: InstrBuffer = new ResizableArray()
        val state = new CodeGenState

        pipeline(ops, instrs, state)

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
                instr.relabel(labels)
                dests(i) = instr
                applyLabels(srcs, labels, dests, n, i + 1, off)
        }
        val instrsOversize = instrs.toArray
        val labelMapping = new Array[Int](state.nlabels)
        val size = findLabels(instrsOversize, labelMapping, instrs.length, 0, 0)
        val instrs_ = new Array[Instr](size)
        applyLabels(instrsOversize, labelMapping, instrs_, instrs_.length, 0, 0)
        instrs_
    }

    final private [parsley] lazy val instrs: Array[Instr] = if (cps) computeInstrs(Cont.ops.asInstanceOf[GenOps]) else safeCall(computeInstrs(_))
    final private lazy val pindices: Array[Int] = instructions.statefulIndices(instrs)
    final private [parsley] def threadSafeInstrs: Array[Instr] = instructions.stateSafeCopy(instrs, pindices)

    // This is a trick to get tail-calls to fire even in the presence of a legimate recursion
    final private [deepembedding] def optimiseDefinitelyNotTailRec: Parsley[A] = optimise

    // Abstracts
    // Sub-tree optimisation and Rec calculation - Bottom-up
    protected def preprocess[Cont[_, +_]: ContOps, A_ >: A](implicit seen: Set[Parsley[_]],
                                                            sub: SubMap,
                                                            label: UnsafeOption[String]): Cont[Unit, Parsley[A_]]
    // Let-finder recursion
    protected def findLetsAux[Cont[_, +_]: ContOps](implicit seen: Set[Parsley[_]], state: LetFinderState): Cont[Unit, Unit]
    // Optimisation - Bottom-up
    protected def optimise: Parsley[A] = this
    // Peephole optimisation and code generation - Top-down
    private [parsley] def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit]
    private [parsley] def prettyASTAux[Cont[_, +_]: ContOps]: Cont[String, String]
}
object Parsley {
    private [Parsley] def allocateRegisters(regs: Set[Reg[_]]): Unit = {
        // Global registers cannot occupy the same slot as another global register
        // In a flatMap, that means a newly discovered global register must be allocated to a new slot
        // This should resize the register pool, but under current restrictions we'll just throw an
        // excepton if there are no available slots
        // For simplicity, we'll demand that the DynCall instruction saves all registers for which it allocated something to that slot
        // This is hopefully a temporary measure
        val unallocatedRegs = regs.filterNot(_.allocated)
        if (unallocatedRegs.nonEmpty) {
            val usedSlots = regs.collect {
                case reg if reg.allocated => reg.addr
            }
            val freeSlots = (0 until 4).filterNot(usedSlots)
            if (unallocatedRegs.size > freeSlots.size) {
                throw new IllegalStateException("Current restrictions require that the maximum number of registers in use is 4")
            }
            for ((reg, addr) <- unallocatedRegs.zip(freeSlots)) reg.allocate(addr)
        }
    }
}

private [deepembedding] trait MZero extends Parsley[Nothing]
private [deepembedding] trait UsesRegister {
    val reg: Reg[_]
}

// Internals
// TODO: Can we remove this SubQueueNode? ListBuffer would be fine using pairs too would be nice.
private [parsley] class CodeGenState {
    import CodeGenState.CodeGenSubQueueNode
    private [this] var current = 0
    private [this] var queue: CodeGenSubQueueNode = _
    val map = mutable.Map.empty[Parsley[_], Int]
    def freshLabel(): Int = {
        val next = current
        current += 1
        next
    }
    def nlabels: Int = current

    def getSubLabel(p: Parsley[_]) = {
        map.getOrElseUpdate(p, {
            queue = new CodeGenSubQueueNode(p, queue)
            freshLabel()
        })
    }

    def nextSub(): Parsley[_] = {
        val p = queue.p
        queue = queue.tail
        p
    }

    def more: Boolean = queue != null
}
private [parsley] object CodeGenState {
    private [CodeGenState] class CodeGenSubQueueNode(val p: Parsley[_], val tail: CodeGenSubQueueNode)
}

private [parsley] class LetFinderState {
    private val _recs = mutable.Set.empty[Parsley[_]]
    private val _preds = mutable.Map.empty[Parsley[_], Int]
    private val _usedRegs = mutable.Set.empty[Reg[_]]

    def addPred(p: Parsley[_]): Unit = _preds += p -> (_preds.getOrElseUpdate(p, 0) + 1)
    def addRec(p: Parsley[_]): Unit = _recs += p
    def addReg(reg: Reg[_]): Unit = _usedRegs += reg
    def notProcessedBefore(p: Parsley[_]): Boolean = _preds(p) == 1

    def lets: Map[Parsley[_], Parsley[_]] = {
        (for ((k, v) <- _preds;
            if v >= 2 && !_recs.contains(k))
        yield k -> {
            val sub = Subroutine(k, null)
            sub.processed = false
            sub
        }).toMap
    }
    def recs: Set[Parsley[_]] = _recs.toSet
    def usedRegs: Set[Reg[_]] = _usedRegs.toSet
}

private [parsley] class SubMap(val subMap: Map[Parsley[_], Parsley[_]]) extends AnyVal {
    def apply[A](p: Parsley[A]): Parsley[A] = subMap.getOrElse(p, p).asInstanceOf[Parsley[A]]
    // $COVERAGE-OFF$
    override def toString: String = subMap.toString
    // $COVERAGE-ON$
}