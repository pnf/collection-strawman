package strawman.collection.immutable

import strawman.collection.{Sorted, SortedLike, SortedPolyTransforms}

/** Base trait for immutable sets whose values have an ordering */
trait SortedSet[A]
  extends Sorted[A]
    with EndoSet[A]
    with SortedSetLike[A, SortedSet]

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends SortedLike[A, C]
    with EndoSetLike[A, C]
    with SortedPolyTransforms[A, C]