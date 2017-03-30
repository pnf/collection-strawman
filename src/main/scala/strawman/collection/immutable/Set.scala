package strawman
package collection.immutable

/** Base trait for immutable set collections */
trait Set[A]
  extends collection.Set[A]
    with Iterable[A]
    with SetLike[A, Set]

/** Base trait for immutable set operations */
trait SetLike[A, +C[X] <: Set[X] with SetLike[X, C]]
  extends collection.SetLike[A, C]
    with SetMonoTransforms[A, C[A]]
    with SetPolyTransforms[A, C]

/** Transformation operations returning a Set containing the same kind of
  * elements
  */
trait SetMonoTransforms[A, +Repr]
  extends collection.SetMonoTransforms[A, Repr] {

  def + (elem: A): Repr

  def - (elem: A): Repr

}

trait SetPolyTransforms[A, +C[X] <: Set[X] with SetLike[X, C]]
  extends collection.SetPolyTransforms[A, C] {

  protected def coll: C[A]

  override def ++ [B >: A](that: collection.IterableOnce[B]): C[B] = {
    var result = coll.asInstanceOf[C[B]] // Note: this is unsound.
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }

}