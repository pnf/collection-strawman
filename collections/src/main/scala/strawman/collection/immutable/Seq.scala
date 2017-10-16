package strawman
package collection
package immutable

import scala.{Any, Int, `inline`}

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]]

trait SeqOps[+A, +CC[_], +C] extends collection.SeqOps[A, CC, C] {

  /** A copy of this $coll with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @tparam B        the element type of the returned $coll.
    *  @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    *
    *    @inheritdoc
    *
    *    @return a copy of this $coll with the element at position `index` replaced by `elem`.
    */
  def updated[B >: A](index: Int, elem: B): CC[B] = fromIterable(View.Updated(toIterable, index, elem))

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
    *
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original $coll appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param  from     the index of the first replaced element
    *  @param  other    the replacement sequence
    *  @param  replaced the number of elements to drop in the original $coll
    *  @tparam B        the element type of the returned $coll.
    *  @return          a new $coll consisting of all elements of this $coll
    *                   except that `replaced` elements starting from `from` are replaced
    *                   by all the elements of `other`.
    */
  def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): CC[B] =
    fromIterable(new View.Patched(toIterable, from, other, replaced))

}

object Seq extends SeqFactory.Delegate[Seq](List)

/** Base trait for immutable indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A]
                        with collection.IndexedSeq[A]
                        with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](Vector)

/** Base trait for immutable indexed Seq operations */
trait IndexedSeqOps[+A, +CC[X] <: IndexedSeq[X], +C] extends SeqOps[A, CC, C] with collection.IndexedSeqOps[A, CC, C]

/** Base trait for immutable linear sequences that have efficient `head` and `tail` */
trait LinearSeq[+A]
  extends Seq[A]
    with collection.LinearSeq[A]
    with LinearSeqOps[A, LinearSeq, LinearSeq[A]]

trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A]]
  extends SeqOps[A, CC, C]
    with collection.LinearSeqOps[A, CC, C]