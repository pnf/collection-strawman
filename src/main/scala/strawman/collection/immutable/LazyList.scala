package strawman
package collection
package immutable

import scala.{None, Nothing, Option, Some, StringContext, Any, Int}
import scala.Predef.???
import scala.annotation.tailrec
import mutable.Builder

class LazyList[+A](expr: => LazyList.Evaluated[A])
  extends Seq[A]
     with LinearSeq[A]
     with SeqOps[A, LazyList, LazyList[A]] {

  private[this] var evaluated = false
  private[this] var result: LazyList.Evaluated[A] = _

  def force: LazyList.Evaluated[A] = {
    if (!evaluated) {
      result = expr
      evaluated = true
    }
    result
  }

  override def isEmpty = force.isEmpty
  override def nonEmpty = force.nonEmpty
  override def head = force.get._1
  override def tail: LazyList[A] = force.get._2

  @tailrec final def length: Int = if (isEmpty) 0 else tail.length

  def #:: [B >: A](elem: B): LazyList[B] = new LazyList(Some((elem, this)))
  def prepend[B >: A](elem: => B): LazyList[B] = new LazyList(Some((elem, this)))

  def iterableFactory = LazyList

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): LazyList[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, LazyList[A]] =
    IndexedSeq.newBuilder().mapResult(_.to(LazyList))

  override def className = "LazyList"

  override def toString =
    if (evaluated)
      result match {
        case None => "Empty"
        case Some((hd, tl)) => s"$hd #:: $tl"
      }
    else "LazyList(?)"
}

object LazyList extends IterableFactory[LazyList] {

  type Evaluated[+A] = Option[(A, LazyList[A])]

  object Empty extends LazyList[Nothing](None)

  object #:: {
    def unapply[A](s: LazyList[A]): Evaluated[A] = s.force
  }

  def fromIterable[A](coll: collection.Iterable[A]): LazyList[A] = coll match {
    case coll: LazyList[A] => coll
    case _ => fromIterator(coll.iterator())
  }

  def fromIterator[A](it: Iterator[A]): LazyList[A] =
    new LazyList(if (it.hasNext) Some(it.next(), fromIterator(it)) else None)

  def empty[A]: LazyList[A] = Empty

}
