package parsley.deepembedding

import scala.language.{higherKinds, implicitConversions}
import scala.annotation.tailrec
import scala.collection.mutable

import parsley.instructions
import instructions.{Stateful, Instr, JumpTable, JumpInstr, Label}
import parsley.{ResizableArray, UnsafeOption}
import ContOps._

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

    final private [parsley] def prettyAST: String = {force(); safeCall((g: GenOps) => perform(prettyASTAux(g))(g))}

    final def unsafe(): Unit = safe = false
    final def force(): Unit = instrs
    final def overflows(): Unit = cps = true

    // Internals
    final private [parsley] def findLets[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        state.addPred(this)
        if (seen(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) findLetsAux(seen + this, state, ops)
        else result(())
    }
    final private [parsley] def fix(implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String]): Parsley[A] =
    {
        // We use the seen set here to prevent cascading sub-routines
        val self = sub(this)
        if (seen(this))
        {
            if (self == this) new DeepEmbedding.Rec(this, label)
            else this
        }
        else self
    }
    final private [parsley] def optimised[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
    {
        for (p <- this.fix.preprocess(seen + this, sub, label, ops)) yield p.optimise
    }
    final private [parsley] var safe = true
    final private [parsley] var cps = false
    final private [parsley] var size: Int = 1
    final private [parsley] var processed = false

    final private def computeInstrs(implicit ops: GenOps): Array[Instr] =
    {
        val instrs: InstrBuffer = new ResizableArray()
        val state = new CodeGenState
        val letFinderState = new LetFinderState
        perform(findLets(Set.empty, letFinderState, ops))
        perform(perform(optimised(Set.empty, new SubMap(letFinderState.lets), null, ops).asInstanceOf[({type C[_, _]})#C[Parsley[_], Parsley[_]]]).codeGen(instrs, state, ops))
        if (state.map.nonEmpty)
        {
            val end = state.freshLabel()
            instrs += new instructions.Jump(end)
            val map = state.map
            while (state.more)
            {
                val p = state.nextSub()
                val label = map(p)
                instrs += new instructions.Label(label)
                perform(p.codeGen(instrs, state, ops))
                instrs += instructions.Return
            }
            instrs += new instructions.Label(end)
        }
        val instrsOversize = instrs.toArray
        val labelMapping = new Array[Int](state.nlabels)
        @tailrec def findLabels(instrs: Array[Instr], labels: Array[Int], n: Int, i: Int, off: Int): Int = if (i + off < n) instrs(i + off) match
        {
            case label: Label => instrs(i + off) = null; labels(label.i) = i; findLabels(instrs, labels, n, i, off + 1)
            case _ => findLabels(instrs, labels, n, i + 1, off)
        }
        else i
        @tailrec def applyLabels(srcs: Array[Instr], labels: Array[Int], dests: Array[Instr], n: Int, i: Int, off: Int): Unit = if (i < n) srcs(i + off) match
        {
            case null => applyLabels(srcs, labels, dests, n, i, off + 1)
            case jump: JumpInstr =>
                jump.label = labels(jump.label)
                dests(i) = jump
                applyLabels(srcs, labels, dests, n, i + 1, off)
            case table: JumpTable =>
                table.relabel(labels)
                dests(i) = table
                applyLabels(srcs, labels, dests, n, i + 1, off)
            case instr =>
                dests(i) = instr
                applyLabels(srcs, labels, dests, n, i + 1, off)
        }
        val size = findLabels(instrsOversize, labelMapping, instrs.length, 0, 0)
        val instrs_ = new Array[Instr](size)
        applyLabels(instrsOversize, labelMapping, instrs_, instrs_.length, 0, 0)
        instrs_
    }

    final private [parsley] lazy val instrs: Array[Instr] = if (cps) computeInstrs(Cont.ops.asInstanceOf[GenOps]) else safeCall(computeInstrs(_))
    final private [this] lazy val pindices: Array[Int] = Parsley.statefulIndices(instrs)
    final private [parsley] def threadSafeInstrs: Array[Instr] = Parsley.stateSafeCopy(instrs, pindices)

    // This is a trick to get tail-calls to fire even in the presence of a legimate recursion
    final private [parsley] def optimiseDefinitelyNotTailRec: Parsley[A] = optimise

    // Abstracts
    // Sub-tree optimisation and Rec calculation - Bottom-up
    protected def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]]
    // Let-finder recursion
    protected def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit]
    // Optimisation - Bottom-up
    private [parsley] def optimise: Parsley[A] = this
    // Peephole optimisation and code generation - Top-down
    private [parsley] def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit]
    private [parsley] def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String]
}

object Parsley
{
    final private [parsley] def pretty(instrs: Array[Instr]): String = {
        val n = instrs.length
        val digits = if (n != 0) Math.log10(n).toInt + 1 else 0
        instrs.zipWithIndex.map {
            case (instr, idx) =>
                val paddedIdx = {
                    val str = idx.toString
                    " " * (digits - str.length) + str
                }
                val paddedHex = {
                    val str = instr.##.toHexString
                    "0" * (8 - str.length) + str
                }
                s"$paddedIdx [$paddedHex]: $instr"
        }.mkString(";\n")
    }

    final private [parsley] def stateSafeCopy(instrs: Array[Instr], pindices: Array[Int]): Array[Instr] =
    {
        val nstateful = pindices.length
        if (nstateful != 0)
        {
            val instrs_ = instrs.clone
            for (i <- 0 until nstateful)
            {
                val j = pindices(i)
                instrs_(j) = instrs(j).copy
            }
            instrs_
        }
        else instrs
    }

    final private [parsley] def statefulIndices(instrs: Array[Instr]): Array[Int] = {
        val buff = new ResizableArray[Int]()
        for (i <- 0 until instrs.length)
        {
            if (instrs(i).isInstanceOf[Stateful]) buff += i
        }
        buff.toArray
    }
}

// Internals
// TODO: Can we remove this SubQueueNode? ListBuffer would be fine using pairs too would be nice.
private [parsley] class CodeGenState
{
    import CodeGenState.CodeGenSubQueueNode
    private [this] var current = 0
    private [this] var queue: CodeGenSubQueueNode = _
    val map = mutable.Map.empty[Parsley[_], Int]
    def freshLabel(): Int =
    {
        val next = current
        current += 1
        next
    }
    def nlabels: Int = current

    def getSubLabel(p: Parsley[_]) =
    {
        map.getOrElseUpdate(p,
        {
            queue = new CodeGenSubQueueNode(p, queue)
            freshLabel()
        })
    }

    def nextSub(): Parsley[_] =
    {
        val p = queue.p
        queue = queue.tail
        p
    }

    def more: Boolean = queue != null
}
private [parsley] object CodeGenState
{
    private [CodeGenState] class CodeGenSubQueueNode(val p: Parsley[_], val tail: CodeGenSubQueueNode)
}

private [parsley] class LetFinderState
{
    private val _recs = mutable.Set.empty[Parsley[_]]
    private val _preds = mutable.Map.empty[Parsley[_], Int]

    def addPred(p: Parsley[_]): Unit = _preds += p -> (_preds.getOrElseUpdate(p, 0) + 1)
    def addRec(p: Parsley[_]): Unit = _recs += p
    def notProcessedBefore(p: Parsley[_]): Boolean = _preds(p) == 1

    def lets: Map[Parsley[_], Parsley[_]] =
    {
        (for ((k, v) <- _preds;
            if v >= 2 && !_recs.contains(k))
        yield k -> {
            val sub = DeepEmbedding.Subroutine(k, null)
            sub.processed = false
            sub
        }).toMap
    }
    def recs: Set[Parsley[_]] = _recs.toSet
}

private [parsley] class SubMap(val subMap: Map[Parsley[_], Parsley[_]]) extends AnyVal
{
    def apply[A](p: Parsley[A]): Parsley[A] = subMap.getOrElse(p, p).asInstanceOf[Parsley[A]]
    override def toString = subMap.toString
}