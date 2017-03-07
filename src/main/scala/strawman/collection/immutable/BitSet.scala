package strawman
package collection.immutable

import strawman.collection.MonomorphicIterableFactory
import strawman.collection.mutable.Builder

import scala.Int
import scala.Predef.???

trait BitSet
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int]
    with SetMonoTransforms[Int, BitSet] // Override mono transforms ops to return a BitSet rather than a SortedSet[Int]

object BitSet extends MonomorphicIterableFactory[Int, BitSet] {

  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSet = ???

  def newBuilder[A <: Int]: Builder[A, BitSet] = ???
}