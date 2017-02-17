package strawman.collection.mutable

import strawman.collection.{EndoIterable, IterableOnce, Iterator, MapFactories}

import scala.{Option, Unit}
import scala.Predef.???

final class HashMap[K, V]
  extends Map[K, V]
    with MapLike[K, V, HashMap] {

  // From IterableOnce
  def iterator(): Iterator[(K, V)] = ???

  // From MapLike
  def get(key: K): Option[V] = ???

  // From Growable
  def +=(elem: (K, V)): this.type = ???
  def clear(): Unit = ???

  // From mutable.MapLike
  def -=(elem: (K, V)): this.type = ???
  def put(key: K, value: V): Option[V] = ???

  // From MapPolyTransforms
  def map[K2, V2](f: (K, V) => (K2, V2)): HashMap[K2, V2] = ???
  def flatMap[K2, V2](f: (K, V) => IterableOnce[(K2, V2)]): HashMap[K2, V2] = ???

  // From IterableMonoTransforms
  protected[this] def fromIterableWithSameElemType(coll: EndoIterable[(K, V)]): HashMap[K, V] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: EndoIterable[B]): Iterable[B] = ??? // Note that here we have to return a mutable Iterable. Does that really make sense?

}

object HashMap extends MapFactories[HashMap] {

  def newBuilder[K, V]: Builder[(K, V), HashMap[K, V]] = ???

}