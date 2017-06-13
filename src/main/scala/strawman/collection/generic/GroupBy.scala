package strawman
package collection
package generic

import strawman.collection.mutable.Builder

object GroupBy {

  /**
    * Generic implementation of `groupBy` that relies on a `Builder`
    * @param f Function that computes the key of an element
    * @param coll Collection on which to apply the `groupBy`
    * @param newBuilder Builder for groups
    * @tparam A Type of elements
    * @tparam K Type of keys
    * @tparam C Type of the collection
    */
  def strict[A, K, C](f: A => K, coll: Iterable[A], newBuilder: () => Builder[A, C]): immutable.Map[K, C] = {
    val m = mutable.Map.empty[K, Builder[A, C]]
    for (elem <- coll) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, newBuilder())
      bldr += elem
    }
    var result = immutable.Map.empty[K, C]
    m.foreach { case (k, v) =>
      result = result + ((k, v.result()))
    }
    result
  }

}
