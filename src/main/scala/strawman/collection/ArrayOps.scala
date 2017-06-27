package strawman
package collection

import scala.{AnyVal, Array, Char, Int}
import scala.Predef.???
import mutable.{ArrayBuffer, GrowableBuilder}

import scala.reflect.ClassTag

class ArrayOps[A](val xs: Array[A])
  extends AnyVal
    with IterableOnce[A]
    with IndexedSeqOps[A, immutable.IndexedSeq, Array[A]]
    with StrictOptimizedIterableOps[A, Array[A]]
    with ArrayLike[A] {

  protected[this] def coll = ArrayView(xs)

  def length = xs.length
  def apply(i: Int) = xs.apply(i)

  override def view = ArrayView(xs)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  def iterableFactory = immutable.IndexedSeq

  protected[this] def fromTaggedIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]
  protected[this] def fromSpecificIterable(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  protected[this] def newSpecificBuilder() = new GrowableBuilder(ArrayBuffer.empty[A]).mapResult(_.toArray(elemTag))

  override def knownSize = xs.length

  override def className = "Array"

  def iterator(): Iterator[A] = coll.iterator()
  def map[B: ClassTag](f: A => B): Array[B] = fromTaggedIterable(View.Map(coll, f))
  def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromTaggedIterable(View.FlatMap(coll, f))
  def ++[B >: A : ClassTag](xs: IterableOnce[B]): Array[B] = fromTaggedIterable(View.Concat(coll, xs))
  def zip[B: ClassTag](xs: IterableOnce[B]): Array[(A, B)] = fromTaggedIterable(View.Zip(coll, xs))
}

case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
  def length = xs.length
  def apply(n: Int) = xs(n)
  override def className = "ArrayView"
}
