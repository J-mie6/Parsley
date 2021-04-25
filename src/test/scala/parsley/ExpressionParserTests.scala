package parsley

import parsley.character.digit
import parsley.implicits.character.{charLift, stringLift}
import parsley.expr.chain
import parsley.expr.{precedence, Ops, GOps, SOps, InfixL, InfixR, Prefix, Postfix, InfixN, Atoms}
import parsley.Parsley._
import parsley._

import scala.language.implicitConversions

class ExpressionParserTests extends ParsleyTest {
    "chain.postfix" must "require an initial value" in {
        chain.postfix('1' #> 1, '+' #> ((x: Int) => x + 1)).parse("1") should be (Success(1))
    }
    it must "parse all operators that follow" in {
        chain.postfix('1' #> 1, '+' #> ((x: Int) => x + 1)).parse("1++++++++++++++") should not be a [Failure[_]]
    }
    it must "apply the functions" in {
        chain.postfix('1' #> 1, '+' #> ((x: Int) => x + 1)).parse("1++++++++++++++") should be (Success(15))
    }
    it must "fail if an operator fails after consuming input" in {
        chain.postfix('1' #> 1, "++" #> ((x: Int) => x + 1)).parse("1+++++++++++++") shouldBe a [Failure[_]]
    }
    it must "not leave the stack in an inconsistent state on failure" in {
        val p = chain.postfix[Int]('1' #> 1, (col.#>[Int => Int](_ + 1)) <* '+')
        val q = chain.left1[Int, Int](p, '*' #> (_ * _))
        noException should be thrownBy q.parse("1+*1+")
    }

    "chain.postfix1" must "require and initial value AND an initial operator" in {
        chain.postfix1('1' #> 1, '+' #> ((x: Int) => x + 1)).parse("1") shouldBe a [Failure[_]]
        chain.postfix1('1' #> 1, '+' #> ((x: Int) => x + 1)).parse("1+") shouldBe Success(2)
    }
    it must "parse all operators that follow" in {
        chain.postfix1('1' #> 1, '+' #> ((x: Int) => x + 1)).parse("1++++++++++++++") should not be a [Failure[_]]
    }
    it must "apply the functions" in {
        chain.postfix1('1' #> 1, '+' #> ((x: Int) => x + 1)).parse("1++++++++++++++") should be (Success(15))
    }
    it must "fail if an operator fails after consuming input" in {
        chain.postfix1('1' #> 1, "++" #> ((x: Int) => x + 1)).parse("1+++++++++++++") shouldBe a [Failure[_]]
    }

    "chain.prefix" must "parse an operatorless value" in {
        chain.prefix('+' #> ((x: Int) => x + 1), '1' #> 1).parse("1") should be (Success(1))
    }
    it must "parse all operators that precede a value" in {
        chain.prefix('+' #> ((x: Int) => x + 1), '1' #> 1).parse("+++++++++++1") should not be a [Failure[_]]
    }
    it must "fail if the final value is absent" in {
        chain.prefix('+' #> ((x: Int) => x + 1), '1' #> 1).parse("+++++++++++") shouldBe a [Failure[_]]
    }
    it must "apply the functions" in {
        chain.prefix('+' #> ((x: Int) => x + 1), '1' #> 1).parse("+++++++++++1") should be (Success(12))
    }

    "chain.prefix1" must "not parse an operatorless value" in {
        chain.prefix1('+' #> ((x: Int) => x + 1), '1' #> 1).parse("1") shouldBe a [Failure[_]]
    }
    it must "parse all operators that precede a value" in {
        chain.prefix1('+' #> ((x: Int) => x + 1), '1' #> 1).parse("+++++++++++1") should not be a [Failure[_]]
    }
    it must "fail if the final value is absent" in {
        chain.prefix1('+' #> ((x: Int) => x + 1), '1' #> 1).parse("+++++++++++") shouldBe a [Failure[_]]
    }
    it must "apply the functions" in {
        chain.prefix1('+' #> ((x: Int) => x + 1), '1' #> 1).parse("+++++++++++1") should be (Success(12))
    }

    "chain.right1" must "require an initial value" in {
        chain.right1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("11") should be (Success(1))
        chain.right1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("1") shouldBe a [Failure[_]]
        chain.right1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("2") shouldBe a [Failure[_]]
    }
    it must "parse all operators and values that follow" in {
        chain.right1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in {
        chain.right1(digit.map(_.asDigit), '%' #> ((x: Int, y: Int) => x % y)).parse("6%5%2%7") should be (Success(0))
    }
    it must "fail if an operator or p fails after consuming input" in {
        chain.right1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).parse("11+11+11+11+11") shouldBe a [Failure[_]]
        chain.right1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).parse("11++11++11++1++11") shouldBe a [Failure[_]]
    }
    it must "correctly accept the use of a wrapping function" in {
        sealed trait Expr
        case class Add(x: Int, y: Expr) extends Expr
        case class Num(x: Int) extends Expr
        val p = chain.right1[Int, Expr]("1" #> 1, "+" #> Add.apply)(Num)
        p.parse("1+1+1") should be (Success(Add(1, Add(1, Num(1)))))
        p.parse("1") should be (Success(Num(1)))
    }
    "chain.right" must "allow for no initial value" in {
        chain.right("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).parse("2") shouldBe Success(0)
        chain.right("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).parse("1") shouldBe a [Failure[_]]
    }

    "chain.left1" must "require an initial value" in {
        chain.left1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("11") should be (Success(1))
        chain.left1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("1") shouldBe a [Failure[_]]
        chain.left1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("2") shouldBe a [Failure[_]]
    }
    it must "parse all operators and values that follow" in {
        chain.left1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).parse("11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in {
        chain.left1(digit.map(_.asDigit), '%' #> ((x: Int, y: Int) => x % y)).parse("6%5%2%7") should be (Success(1))
    }
    it must "fail if an operator fails after consuming input" in {
        chain.left1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).parse("11+11+11+11+11") shouldBe a [Failure[_]]
        chain.left1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).parse("11++11++11++1++11") shouldBe a [Failure[_]]
    }
    it must "not leave the stack in an inconsistent state on failure" in {
        val p = chain.left1[Int, Int]('1' #> 1, (col.#>[(Int, Int) => Int](_ + _)) <* '+')
        val q = chain.left1[Int, Int](p, '*' #> (_ * _))
        noException should be thrownBy q.parse("1+1*1+1")
    }
    it must "correctly accept the use of a wrapping function" in {
        sealed trait Expr
        case class Add(x: Expr, y: Int) extends Expr
        case class Num(x: Int) extends Expr
        chain.left1[Int, Expr]("1" #> 1, "+" #> Add.apply)(Num).parse("1+1+1") should be (Success(Add(Add(Num(1), 1), 1)))
    }
    "chain.left" must "allow for no initial value" in {
        chain.left("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).parse("11") should be (Success(1))
        chain.left("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).parse("1") shouldBe a [Failure[_]]
        chain.left("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).parse("2") shouldBe Success(0)
    }

    "expression parsers" should "result in correct precedence" in {
        val expr = precedence[Int](digit.map(_.asDigit))(Ops(InfixL)('*' #> (_*_)),
                                                         Ops(InfixL)('+' #> (_+_)))
        expr.parse("1+2*3+4") should be (Success(11))
        expr.parse("1*2+3*4") should be (Success(14))
    }
    they should "work for multiple operators at the same level" in {
        val expr = precedence[Int](digit.map(_.asDigit))(Ops(InfixL)('+' #> (_+_), '-' #> (_-_)))
        expr.parse("1+2-3+4") should be (Success(4))
        expr.parse("1-2+3-4") should be (Success(-2))
    }
    they should "work for mixed associativity operators" in {
        val expr = precedence[Int](digit.map(_.asDigit))(Ops(InfixL)('*' #> (_*_)),
                                                         Ops(InfixR)('+' #> (_+_)))
        expr.parse("1+2*3+4") should be (Success(11))
        expr.parse("1*2+3*4") should be (Success(14))
    }
    they should "parse mathematical expressions" in {
        lazy val expr: Parsley[Int] = precedence[Int](atom)(
            Ops(Prefix)('-' #> (x => -x)),
            Ops(InfixL)('/' #> (_/_)),
            Ops(InfixR)('*' #> (_*_)),
            Ops(InfixL)('+' #> (_+_), '-' #> (_-_)))
        lazy val atom: Parsley[Int] = digit.map(_.asDigit) <|> ('(' *> expr <* ')')
        expr.parse("(2+3)*8") should be (Success(40))
        expr.parse("-3+4") should be (Success(1))
        expr.parse("-(3+4)") should be (Success(-7))
        expr.parse("(3+-7)*(-2--4)/2") should be (Success(-4))
    }
    they should "parse prefix operators mixed with infix operators" in {
        lazy val expr = precedence[Int](atom)(Ops(Prefix)('-' #> (x => -x)),
                                              Ops(InfixL)('-' #> (_-_)))
        lazy val atom: Parsley[Int] = digit.map(_.asDigit) <|> ('(' *> expr <* ')')
        expr.parse("-1") should be (Success(-1))
        expr.parse("2-1") should be (Success(1))
        expr.parse("-2-1") should be (Success(-3))
        expr.parse("-(2-1)") should be (Success(-1))
        expr.parse("(-0)-1") should be (Success(-1))
    }
    they should "be able to parse prefix operators weaker than an infix" in {
        sealed trait Expr
        case class Lt(x: Expr, y: Expr) extends Expr
        case class Inc(x: Expr) extends Expr
        case class Num(x: Int) extends Expr
        val expr = precedence[Expr](digit.map(_.asDigit).map(Num))(Ops(InfixL)('<' #> Lt),
                                                                   Ops(Prefix)("++" #> Inc))
        expr.parse("++1<2") should be (Success(Inc(Lt(Num(1), Num(2)))))
    }
    they should "generalise to sub-typed structures" in {
        sealed trait Comp
        case class Less(x: Expr, y: Expr) extends Comp
        sealed trait Expr extends Comp
        case class Add(x: Expr, y: Term) extends Expr
        sealed trait Term extends Expr
        case class Mul(x: Factor, y: Term) extends Term
        sealed trait Factor extends Term
        case class Neg(x: Factor) extends Factor
        sealed trait Atom extends Factor
        case class Parens(x: Comp) extends Atom
        case class Num(x: Int) extends Atom
        lazy val expr: Parsley[Comp] = precedence(
            SOps(InfixN)('<' #> Less) +:
            SOps(InfixL)('+' #> Add) +:
            SOps(InfixR)('*' #> Mul) +:
            SOps[Factor, Atom](Prefix)('-' #> Neg) +:
            Atoms(digit.map(_.asDigit).map(Num), '(' *> expr.map(Parens) <* ')'))
        expr.parse("(7+8)*2+3+6*2") should be (Success(Add(Add(Mul(Parens(Add(Num(7), Num(8))), Num(2)), Num(3)), Mul(Num(6), Num(2)))))
    }
    they should "generalise to non-monolithic structures" in {
        sealed trait Comp
        case class Less(x: Expr, y: Expr) extends Comp
        case class CompOf(x: Expr) extends Comp
        sealed trait Expr
        case class Add(x: Expr, y: Term) extends Expr
        case class ExprOf(x: Term) extends Expr
        sealed trait Term
        case class Mul(x: Atom, y: Term) extends Term
        case class TermOf(x: Atom) extends Term
        sealed trait Atom
        case class Parens(x: Comp) extends Atom
        case class Num(x: Int) extends Atom
        lazy val expr: Parsley[Comp] = precedence(
            GOps[Expr, Comp](InfixN)('<' #> Less)(CompOf) +:
            GOps[Term, Expr](InfixL)('+' #> Add)(ExprOf) +:
            GOps[Atom, Term](InfixR)('*' #> Mul)(TermOf) +:
            Atoms(digit.map(_.asDigit).map(Num), '(' *> expr.map(Parens) <* ')'))
        expr.parse("(7+8)*2+3+6*2<4") should be {
            Success(Less(
                Add(
                    Add(
                        ExprOf(
                            Mul(
                                Parens(CompOf(Add(ExprOf(TermOf(Num(7))), TermOf(Num(8))))),
                                TermOf(Num(2)))),
                        TermOf(Num(3))),
                    Mul(Num(6), TermOf(Num(2)))),
                ExprOf(TermOf(Num(4)))))
        }
    }
    they should "generalise to non-monolithic structures with more than one chainl1" in {
        sealed trait Expr
        case class Add(x: Expr, y: Term) extends Expr
        case class ExprOf(x: Term) extends Expr
        sealed trait Term
        case class Mul(x: Term, y: Atom) extends Term
        case class TermOf(x: Atom) extends Term
        sealed trait Atom
        case class Parens(x: Expr) extends Atom
        case class Num(x: Int) extends Atom
        lazy val expr: Parsley[Expr] = precedence(
            Atoms(digit.map(_.asDigit).map(Num), '(' *> expr.map(Parens) <* ')') :+
            GOps[Atom, Term](InfixL)('*' #> Mul)(TermOf) :+
            GOps[Term, Expr](InfixL)('+' #> Add)(ExprOf))
        expr.parse("1*(2+3)") shouldBe a [Success[_]]
    }

    "mixed expressions" should "also be parsable" in {
        val lang = token.LanguageDef.plain.copy(
            identStart = token.Predicate(_.isLetter),
            identLetter = token.Predicate(_.isLetter)
        )

        sealed trait Expr
        case class Binary(l: Expr, r: Expr) extends Expr
        case class Unary(c: Expr) extends Expr
        case class Constant(x: String) extends Expr

        val tok = new token.Lexer(lang)

        lazy val ops: Seq[Ops[Expr, Expr]] = Seq(
            Ops(InfixR)(',' #> Binary),
            Ops(InfixR)(".=" #> Binary),
            Ops(InfixL)('.' #> Binary),
            Ops(Postfix)(tok.parens(expr </> Constant("")).map(e1 => Binary(_, e1)))
        )

        lazy val atom: Parsley[Expr] = tok.identifier.map(Constant)
        lazy val expr: Parsley[Expr] = precedence(ops: _*)(atom)

        expr.parse("o.f()") shouldBe a [Success[_]]
        expr.parse("o.f(x,y)") shouldBe a [Success[_]]
    }
}
