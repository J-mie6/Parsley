package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.{UnsafeOption, instructions}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private [deepembedding] sealed abstract class ManyLike[A, B](_p: =>Parsley[A], name: String, empty: =>ManyLike[A, B], unit: B, instr: Int => instructions.Instr)
    extends Unary[A, B](_p)(c => s"$name($c)", _ => empty) {
    final override val numInstrs = 2
    final override def optimise: Parsley[B] = p match {
        case _: Pure[_] => throw new Exception(s"$name given parser which consumes no input")
        case _: MZero => new Pure(unit)
        case _ => this
    }
    final override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += instr(body)
        }
    }
}
private [parsley] final class Many[A](_p: =>Parsley[A]) extends ManyLike[A, List[A]](_p, "many", Many.empty, Nil, new instructions.Many(_))
private [parsley] final class SkipMany[A](_p: =>Parsley[A]) extends ManyLike[A, Unit](_p, "skipMany", SkipMany.empty, (), new instructions.SkipMany(_))
private [deepembedding] sealed abstract class ChainLike[A](_p: =>Parsley[A], _op: =>Parsley[A => A], pretty: (String, String) => String, empty: =>ChainLike[A])
    extends Binary[A, A => A, A](_p, _op)(pretty, empty) {
    override def optimise: Parsley[A] = right match {
        case _: Pure[_] => throw new Exception("chain given parser which consumes no input")
        case _: MZero => left
        case _ => this
    }
}
private [parsley] final class ChainPost[A](_p: =>Parsley[A], _op: =>Parsley[A => A])
    extends ChainLike[A](_p, _op, (l, r) => s"chainPost($l, $r)", ChainPost.empty) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.ChainPost(body)
            }
        }
    }
}
private [parsley] final class ChainPre[A](_p: =>Parsley[A], _op: =>Parsley[A => A])
    extends ChainLike[A](_p, _op, (l, r) => s"chainPre($r, $l)", ChainPre.empty) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        right.codeGen >> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ChainPre(body)
            left.codeGen |>
            (instrs += instructions.Apply)
        }
    }
}
private [parsley] final class Chainl[A, B](_init: Parsley[B], _p: =>Parsley[A], _op: =>Parsley[(B, A) => B])
    extends Ternary[B, A, (B, A) => B, B](_init, _p, _op)((f, s, t) => s"chainl1($s, $t)", Chainl.empty) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        first.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            third.codeGen >>
            second.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainl(body)
            }
        }
    }
}
private [parsley] final class Chainr[A, B](_p: =>Parsley[A], _op: =>Parsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](_p, _op)((l, r) => s"chainr1($l, $r)", Chainr.empty(wrap)) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit]= {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainr(body, wrap)
            }
        }
    }
}
private [parsley] final class SepEndBy1[A, B](_p: =>Parsley[A], _sep: =>Parsley[B])
    extends Binary[A, B, List[A]](_p, _sep)((l, r) => s"sepEndBy1($r, $l)", SepEndBy1.empty) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.SepEndBy1(body)
            }
        }
    }
}
private [parsley] final class ManyUntil[A](_body: Parsley[Any]) extends Unary[Any, List[A]](_body)(c => s"manyUntil($c)", _ => ManyUntil.empty) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        val start = state.freshLabel()
        val loop = state.freshLabel()
        instrs += new instructions.PushFallthrough(loop)
        instrs += new instructions.Label(start)
        p.codeGen |> {
            instrs += new instructions.Label(loop)
            instrs += new instructions.ManyUntil(start)
        }
    }
}

private [deepembedding] object Many {
    def empty[A]: Many[A] = new Many(null)
    def apply[A](p: Parsley[A]): Many[A] = empty.ready(p)
}
private [deepembedding] object SkipMany {
    def empty[A]: SkipMany[A] = new SkipMany(null)
    def apply[A](p: Parsley[A]): SkipMany[A] = empty.ready(p)
}
private [deepembedding] object ChainPost {
    def empty[A]: ChainPost[A] = new ChainPost(null, null)
    def apply[A](left: Parsley[A], right: Parsley[A => A]): ChainPost[A] = empty.ready(left, right)
}
private [deepembedding] object ChainPre {
    def empty[A]: ChainPre[A] = new ChainPre(null, null)
    def apply[A](left: Parsley[A], right: Parsley[A => A]): ChainPre[A] = empty.ready(left, right)
}
private [deepembedding] object Chainl {
    def empty[A, B]: Chainl[A, B] = new Chainl(null, null, null)
    def apply[A, B](first: Parsley[B], second: Parsley[A], third: Parsley[(B, A) => B]): Chainl[A, B] = empty.ready(first, second, third)
}
private [deepembedding] object Chainr {
    def empty[A, B](wrap: A => B): Chainr[A, B] = new Chainr(null, null, wrap)
    def apply[A, B](left: Parsley[A], right: Parsley[(A, B) => B], wrap: A => B): Chainr[A, B] = empty(wrap).ready(left, right)
}
private [deepembedding] object SepEndBy1 {
    def empty[A, B]: SepEndBy1[A, B] = new SepEndBy1(null, null)
    def apply[A, B](left: Parsley[A], right: Parsley[B]): SepEndBy1[A, B] = empty.ready(left, right)
}
private [parsley] object ManyUntil {
    object Stop
    def empty[A]: ManyUntil[A] = new ManyUntil(null)
    def apply[A](p: Parsley[Any]): ManyUntil[A] = empty.ready(p)
}