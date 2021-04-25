package parsley.expr

import parsley.Parsley

/**
 * Describes the operators at a specific level in the precedence tree, such that these ops
 * consume `B`s, possibly `A`s and produce `B`s: this depends on the [[Fixity]] of the operators.
 * @tparam A The base type consumed by the operators
 * @tparam B The type produced/consumed by the operators
 * @note For less complex types `Ops[A, A]` is sufficient
 * @since 2.2.0
 */
trait Ops[-A, B] {
    private [expr] val wrap: A => B
}
private [expr] case class Lefts[-A, B](ops: Parsley[(B, A) => B]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Rights[-A, B](ops: Parsley[(A, B) => B]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Prefixes[-A, B](ops: Parsley[B => B]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Postfixes[-A, B](ops: Parsley[B => B]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class NonAssocs[-A, B](ops: Parsley[(A, A) => B]*)(implicit override val wrap: A => B) extends Ops[A, B]

/**
 * Helper object to build values of `Ops[A, B]`, for generalised precedence parsing
 * @since 2.2.0
 */
object GOps {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.GOp[A, B]`, NOT `GOp[A, B]`. Builds an `Ops` object which represents many operators
    * which act at the same precedence level, with a given fixity. Using path-dependent typing,
    * the given fixity describes the shape of the operators expected. For more information see
    * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
    * @tparam A The base type consumed by the operators
    * @tparam B The type produced/consumed by the operators
    * @param fixity The fixity of the operators described. See [[Fixity]]
    * @param ops The operators themselves, in varargs
    * @param wrap The function which should be used to wrap up a value of type `A` when required
    *             (this will be at right of a left-assoc chain, left of a right-assoc chain, or
    *             the root of a prefix/postfix chain)
    * @since 2.2.0
    */
    def apply[A, B](fixity: Fixity)(ops: Parsley[fixity.GOp[A, B]]*)(implicit wrap: A => B): Ops[A, B] = fixity match {
        case InfixL  => Lefts[A, B](ops.asInstanceOf[Seq[Parsley[InfixL.GOp[A, B]]]]: _*)
        case InfixR  => Rights[A, B](ops.asInstanceOf[Seq[Parsley[InfixR.GOp[A, B]]]]: _*)
        case Prefix  => Prefixes[A, B](ops.asInstanceOf[Seq[Parsley[Prefix.GOp[A, B]]]]: _*)
        case Postfix => Postfixes[A, B](ops.asInstanceOf[Seq[Parsley[Postfix.GOp[A, B]]]]: _*)
        case InfixN  => NonAssocs[A, B](ops.asInstanceOf[Seq[Parsley[InfixN.GOp[A, B]]]]: _*)
    }
}

/**
 * Helper object to build values of `Ops[A, B]`, for precedence parsing with subtyped data-structures.
 * @since 3.0.0
 */
object SOps {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.SOp[A, B]`, NOT `SOp[A, B]`. Builds an `Ops` object which represents many operators
    * which act at the same precedence level, with a given fixity. Using path-dependent typing,
    * the given fixity describes the shape of the operators expected. For more information see
    * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
    * @tparam B The type produced/consumed by the operators, must be a supertype of `A`
    * @tparam A The base type consumed by the operators
    * @param fixity The fixity of the operators described. See [[Fixity]]
    * @param ops The operators themselves, in varargs
    * @since 3.0.0
    * @note The order of types in this method is reversed compared with [[GOps.apply]], this is due to
    *       a Scala typing issue.
    */
    def apply[B, A <: B](fixity: Fixity)(ops: Parsley[fixity.SOp[A, B]]*): Ops[A, B] = GOps(fixity)(ops: _*)
}

/**
 * Helper object to build values of `Ops[A, A]`, for monolithic precedence parsing
 * @since 2.2.0
 */
object Ops {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.Op[A]`, NOT `Op[A]`. Builds an `Ops` object which represents many operators
    * which act at the same precedence level, with a given fixity. Using path-dependent typing,
    * the given fixity describes the shape of the operators expected. For more information see
    * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
    * @tparam A The type associated with the operators (which it consumes and produces)
    * @param fixity The fixity of the operators described. See [[Fixity]]
    * @param ops The operators themselves, in varargs
    * @since 2.2.0
    */
    def apply[A](fixity: Fixity)(ops: Parsley[fixity.Op[A]]*): Ops[A, A] = GOps[A, A](fixity)(ops: _*)
}