package parsley.expr

import scala.language.higherKinds

/**
  * Denotes the fixity and associativity of an operator. Importantly, it also specifies the type of the
  * of the operations themselves. For non-monolithic structures this is described by `fixity.GOp`, subtyped
  * structures `fixity.SOp`, and for monolithic structures `fixity.Op`.
  * @since 2.2.0
  */
sealed trait Fixity {
    type Op[A] = GOp[A, A]
    type SOp[A, B >: A] <: GOp[A, B]
    type GOp[-A, B]
}

/**
  * Describes left-associative binary operators
  * @since 2.2.0
  */
case object InfixL extends Fixity {
    override type GOp[-A, B] = (B, A) => B
    override type SOp[-A, B >: A] = (B, A) => B
}

/**
  * Describes right-associative binary operators
  * @since 2.2.0
  */
case object InfixR extends Fixity {
    override type GOp[-A, B] = (A, B) => B
    override type SOp[-A, B >: A] = (A, B) => B
}

/**
  * Describes unary prefix operators
  * @since 2.2.0
  */
case object Prefix extends Fixity {
    override type GOp[-A, B] = B => B
    override type SOp[A, B >: A] = B => B
}

/**
  * Describes unary postfix operators
  * @since 2.2.0
  */
case object Postfix extends Fixity {
    override type GOp[-A, B] = B => B
    override type SOp[A, B >: A] = B => B
}

/**
  * Describes non-associative operators
  * @since 4.0.0
  */
case object InfixN extends Fixity {
    override type GOp[-A, +B] = (A, A) => B
    override type SOp[-A, B >: A] = (A, A) => B
}