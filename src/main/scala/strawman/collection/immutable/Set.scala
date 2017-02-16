package strawman.collection.immutable

import strawman.collection.IterableLike

import scala.Any

/** Base trait for immutable set operations supporting only monomorphic transformations */
trait EndoSet[A]
  extends strawman.collection.EndoSet[A]
    with EndoSetLike[A, EndoSet]

trait EndoSetLike[A, +C[X] <: EndoSet[X]]
  extends strawman.collection.EndoSetLike[A, C] {

  def + (elem: A): C[A]

  def - (elem: A): C[A]

}

/** Base trait for immutable set collections */
trait Set[A]
  extends strawman.collection.Set[A]
    with Iterable[A]
    with EndoSet[A]
    with SetLike[A, Set]


/** Base trait for immutable set operations */
trait SetLike[A, +C[X] <: Set[X]]
  extends strawman.collection.SetLike[A, C]
    with EndoSetLike[A, C]