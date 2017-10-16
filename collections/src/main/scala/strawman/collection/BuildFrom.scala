package strawman.collection

import scala.{Any, Ordering}

import strawman.collection.mutable.Builder

/** Builds a collection of type `C` from elements of type `A` when a source collection of type `From` is available.
  * Implicit instances of `BuildFrom` are available for all collection types.
  *
  * @tparam From Type of source collection
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait BuildFrom[-From, -A, +C] extends Any {
  def fromSpecificIterable(from: From)(it: Iterable[A]): C

  /** Get a Builder for the collection. For non-strict collection types this will use an intermediate buffer.
    * Building collections with `fromSpecificIterable` is preferred because it can be lazy for lazy collections. */
  def newBuilder(from: From): Builder[A, C]
}

object BuildFrom extends BuildFromLowPriority {
  /** Build the source collection type from a MapOps */
  implicit def buildFromMapOps[CC[X, Y] <: Map[X, Y] with MapOps[X, Y, CC, _], K0, V0, K, V]: BuildFrom[CC[K0, V0], (K, V), CC[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = from.mapFactory.newBuilder[K, V]()
    def fromSpecificIterable(from: CC[K0, V0])(it: Iterable[(K, V)]): CC[K, V] = from.mapFactory.from(it)
  }

  /** Build the source collection type from a SortedMapOps */
  implicit def buildFromSortedMapOps[CC[X, Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], K0, V0, K : Ordering, V]: BuildFrom[CC[K0, V0], (K, V), CC[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = from.sortedMapFactory.newBuilder[K, V]()
    def fromSpecificIterable(from: CC[K0, V0])(it: Iterable[(K, V)]): CC[K, V] = from.sortedMapFactory.from(it)
  }

  /** Build the source collection type from an Iterable with SortedOps */
  implicit def buildFromSortedSetOps[CC[X] <: SortedSet[X] with SortedSetOps[X, CC, _], A0, A : Ordering]: BuildFrom[CC[A0], A, CC[A]] = new BuildFrom[CC[A0], A, CC[A]] {
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = from.sortedIterableFactory.newBuilder[A]()
    def fromSpecificIterable(from: CC[A0])(it: Iterable[A]): CC[A] = from.sortedIterableFactory.from(it)
  }
}

trait BuildFromLowPriority {
  /** Build the source collection type from an IterableOps */
  implicit def buildFromIterableOps[CC[X] <: Iterable[X] with IterableOps[X, CC, _], A0, A]: BuildFrom[CC[A0], A, CC[A]] = new BuildFrom[CC[A0], A, CC[A]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = from.iterableFactory.newBuilder[A]()
    def fromSpecificIterable(from: CC[A0])(it: Iterable[A]): CC[A] = from.iterableFactory.from(it)
  }
}
