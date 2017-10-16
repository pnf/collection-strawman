package strawman
package collection

import scala.{AnyVal, Array, ArrayIndexOutOfBoundsException, Char, Int, throws}
import scala.Predef.???
import mutable.{ArrayBuffer, GrowableBuilder}

import scala.reflect.ClassTag

class ArrayOps[A](val xs: Array[A])
  extends AnyVal
    with IterableOnce[A]
    with IndexedSeqOps[A, immutable.IndexedSeq, Array[A]]
    with StrictOptimizedIterableOps[A, Seq, Array[A]]
    with ArrayLike[A] {

  def toIterable = ArrayView(xs)
  protected[this] def coll: Array[A] = xs
  def toSeq: Seq[A] = fromIterable(toIterable)

  def length = xs.length
  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int) = xs.apply(i)

  override def view = ArrayView(xs)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  def iterableFactory = immutable.IndexedSeq

  protected[this] def fromTaggedIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]
  protected[this] def fromSpecificIterable(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  protected[this] def newSpecificBuilder() = ArrayBuffer.newBuilder[A]().mapResult(_.toArray(elemTag))

  override def knownSize = xs.length

  override def className = "Array"

  def map[B: ClassTag](f: A => B): Array[B] = fromTaggedIterable(View.Map(toIterable, f))

  def mapInPlace(f: A => A): Array[A] = {
    var i = 0
    while (i < xs.length) {
      xs.update(i, f(xs(i)))
      i = i + 1
    }
    xs
  }

  def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromTaggedIterable(View.FlatMap(toIterable, f))

  def ++[B >: A : ClassTag](xs: Iterable[B]): Array[B] = fromTaggedIterable(View.Concat(toIterable, xs))

  def zip[B: ClassTag](xs: Iterable[B]): Array[(A, B)] = fromTaggedIterable(View.Zip(toIterable, xs))

}

case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
  def length = xs.length
  @throws[ArrayIndexOutOfBoundsException]
  def apply(n: Int) = xs(n)
  override def className = "ArrayView"
}
