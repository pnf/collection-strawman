package strawman.collection

import strawman.collection.mutable.Builder

import scala.{Any, Ordering}

/**
  * Auxiliary type allowing us to define which collection type to build according to the type of an existing collection.
  *
  * @tparam From Existing collection type that will drives the collection type to build
  * @tparam A Element type
  */
trait BuildFrom[-From, -A] {

  /** Collection type to build */
  type To

  def newBuilder(): Builder[A, To]

}

trait BuildFromLowPriority {

  /**
    * Extracts the binary type constructor of `From` and applies it to `A` and `B`
    * to build the `To` type.
    */
  implicit def binaryTC[CC[_, _], A, B](implicit
    cb: CanBuild[(A, B), CC[A, B]]
  ): BuildFrom.Aux[CC[_, _], (A, B), CC[A, B]] =
    new BuildFrom[CC[_, _], (A, B)] {
      type To = CC[A, B]
      def newBuilder(): Builder[(A, B), CC[A, B]] = cb.newBuilder()
    }

}

object BuildFrom extends BuildFromLowPriority {

  type Aux[From, A, To0] = BuildFrom[From, A] { type To = To0 }

  /**
    * Extracts the unary type constructor of `From` and applies it to `A`
    * to build the `To` type.
    */
  implicit def unaryTC[CC[_], A](implicit
    cb: CanBuild[A, CC[A]]
  ): BuildFrom.Aux[CC[_], A, CC[A]] =
    new BuildFrom[CC[_], A] {
      type To = CC[A]
      def newBuilder(): Builder[A, CC[A]] = cb.newBuilder()
    }

  // Explicit BuildFrom instances allowing breakOut-like style (see the tests for usage examples)
  // We have to define four variants to support all the combinations of factories (unary vs binary type
  // constructor, ordered vs unordered)

  def factory[CC[_], A](iterableFactory: IterableFactoryWithBuilder[CC]): BuildFrom.Aux[Any, A, CC[A]] =
    new BuildFrom[Any, A] {
      type To = CC[A]
      def newBuilder(): Builder[A, CC[A]] = iterableFactory.newBuilder[A]()
    }

  def factory[CC[_], A](sortedIterableFactory: SortedIterableFactoryWithBuilder[CC])(implicit ordering: Ordering[A]): BuildFrom.Aux[Any, A, CC[A]] =
    new BuildFrom[Any, A] {
      type To = CC[A]
      def newBuilder(): Builder[A, CC[A]] = sortedIterableFactory.newBuilder[A]()
    }

  def factory[CC[X, Y] <: Map[X, Y] with MapOps[X, Y, CC, _], K, V](
    mapFactory: MapFactoryWithBuilder[CC]
  ): BuildFrom.Aux[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V)] {
      type To = CC[K, V]
      def newBuilder(): Builder[(K, V), CC[K, V]] = mapFactory.newBuilder[K, V]()
    }

  def factory[CC[X, Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], K, V](
    sortedMapFactory: SortedMapFactoryWithBuilder[CC]
  )(implicit
    ordering: Ordering[K]
  ): BuildFrom.Aux[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V)] {
      type To = CC[K, V]
      def newBuilder(): Builder[(K, V), CC[K, V]] = sortedMapFactory.newBuilder[K, V]()
    }

}
