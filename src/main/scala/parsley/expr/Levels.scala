package parsley.expr

import parsley.Parsley

/**
  * For more complex expression parser types `Levels` can be used to
  * describe the precedence table whilst preserving the intermediate
  * structure between each level.
  * @tparam A The type of structure produced by the list of levels
  * @since 4.0.0
  */
/* FIXME: This should be covariant! */
sealed trait Levels[A] {
    /**
      * Builds a larger precedence table from strongest to weakest
      * @tparam B The new result type for the larger table
      * @param ops The operators that transform the previous, stronger, layer into the new result
      */
    final def :+[B](ops: Ops[A, B]): Levels[B] = Level(this, ops)
    /**
      * Builds a larger parser precedence table from weakest to strongest
      * @tparam B The new result type for the larger table
      * @param ops The operators that transform the next, stronger, layer into the new result
      */
    final def +:[B](ops: Ops[A, B]): Levels[B] = Level(this, ops)
}
/**
  * This represents a single new level of the hierarchy, with stronger
  * precedence than its tail.
  * @tparam A The intermediate type produced by the layer below to be fed into this level
  * @tparam B The type of structure produced by this layer
  * @param ops The operators accepted at this level
  * @param lvls The next, stronger, levels in the precedence table
  * @return A larger precedence table transforming atoms of type `A` into
  *          a structure of type `B`.
  * @since 4.0.0
  */
case class Level[A, B](lvls: Levels[A], ops: Ops[A, B]) extends Levels[B]
/**
  * Given some atoms, produces the base of the precedence hierarchy
  * @tparam A The base type of the hierarchy
  * @param atoms The atoms at the bottom of the precedence table
  * @since 4.0.0
  */
case class Atoms[A](atoms: Parsley[A]*) extends Levels[A]