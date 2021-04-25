package parsley

import scala.collection.mutable
import scala.language.higherKinds

private [parsley] object XCompat {
    def applyWrap[A, B](f: A => B)(p: Parsley[A]): Parsley[B] = f match {
        case refl: (A <:< B) => refl.substituteCo[Parsley](p)
        case refl: (A =:= B) => refl.substituteCo[Parsley](p)
        case wrap => p.map(wrap)
    }

    private implicit class SubtitutionEq[A, B](ev: A =:= B) {
        def substituteCo[F[_]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
    }

    private implicit class SubtitutionSub[A, B](ev: A <:< B) {
        def substituteCo[F[_]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
    }

    implicit class MapValuesInPlace[K, V](m: mutable.Map[K, V]) {
        def mapValuesInPlace(f: (K, V) => V): mutable.Map[K, V] = m.transform(f)
    }
}
