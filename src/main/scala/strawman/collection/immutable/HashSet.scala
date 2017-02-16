package strawman.collection.immutable

import strawman.collection.{EndoIterable, IterableFactory, Iterator}

import scala.Boolean
import scala.Predef.???

/** An immutable Set backed by a hash trie */
final class HashSet[A]
  extends Set[A]
    with SetLike[A, HashSet] {

  // from IterablePolyTransforms
  def fromIterable[B](it: EndoIterable[B]): HashSet[B] =
    HashSet.fromIterable(it)

  // from IterableOnce
  def iterator(): Iterator[A] = ???

  // from MonoSet
  def & (that: strawman.collection.EndoSet[A]): HashSet[A] = ???
  def ++ (that: strawman.collection.EndoSet[A]): HashSet[A] = ???

  // from immutable.MonoSet
  def + (elem: A): HashSet[A] = ???
  def - (elem: A): HashSet[A] = ???
  def contains(elem: A): Boolean = ???

}

object HashSet extends IterableFactory[HashSet] {

  def fromIterable[B](it: EndoIterable[B]): HashSet[B] = ???

}