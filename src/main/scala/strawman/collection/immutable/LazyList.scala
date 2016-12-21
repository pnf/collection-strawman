package strawman.collection.immutable

import scala.{Int, None, Nothing, Option, Some, StringContext}
import strawman.collection
import strawman.collection.{IterableFactory, LinearSeq, SeqLikeFromIterable, Iterator}

class LazyList[+A](expr: => LazyList.Evaluated[A])
  extends LinearSeq[A]
    with SeqLikeFromIterable[A, LazyList] {
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
  override def tail = force.get._2

  def #:: [B >: A](elem: => B): LazyList[B] = new LazyList(Some((elem, this)))

  def fromIterable[B](c: collection.Iterable[B]): LazyList[B] = LazyList.fromIterable(c)

  override def className = "LazyList"

  override def toString =
    if (evaluated)
      result match {
        case None => "Empty"
        case Some((hd, tl)) => s"$hd #:: $tl"
      }
    else "LazyList(?)"

  // HACK Needed because we inherit from two implementations (one from LinearSeq and one from SeqLikeFromIterable)
  override def drop(n: Int): LazyList[A] = super.drop(n)

}

object LazyList extends IterableFactory[LazyList] {

  type Evaluated[+A] = Option[(A, LazyList[A])]

  object Empty extends LazyList[Nothing](None)

  object #:: {
    def unapply[A](s: LazyList[A]): Evaluated[A] = s.force
  }

  def fromIterable[B](coll: collection.Iterable[B]): LazyList[B] = coll match {
    case coll: LazyList[B] => coll
    case _ => fromIterator(coll.iterator())
  }

  def fromIterator[B](it: Iterator[B]): LazyList[B] =
    new LazyList(if (it.hasNext) Some(it.next(), fromIterator(it)) else None)
}
