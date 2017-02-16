package strawman.collection

import scala.Boolean

/** Partial base trait for set collections
  *
  * Supports only operations returning a set with
  * elements of the same type `A`.
  */
trait EndoSet[A]
  extends EndoIterable[A]
    with EndoSetLike[A, EndoSet]

/**
  * Operations of endomorphic sets.
  */
trait EndoSetLike[A, +C[X] <: EndoSet[X]]
  extends EndoIterableLike[A, C]
    with SetMonoTransforms[A, C[A]] {

  def contains(elem: A): Boolean

}

/** Monomorphic transformation operations on sets */
trait SetMonoTransforms[A, +Repr] {

  def filter(p: A => Boolean): Repr

  /** Intersection of `this` and `that` */
  def & (that: EndoSet[A]): Repr

  /** Union of `this` and `that` */
  def ++ (that: EndoSet[A]): Repr

}

/** Base trait for set collections */
trait Set[A]
  extends EndoSet[A]
    with Iterable[A]
    with SetLike[A, Set]

/** Base trait for set operations */
trait SetLike[A, +C[X] <: Set[X]]
  extends EndoSetLike[A, C]
    with IterableLike[A, C]
