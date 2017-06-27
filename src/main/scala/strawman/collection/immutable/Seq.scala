package strawman
package collection
package immutable

import scala.{Any, Int, `inline`}

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]]

trait SeqOps[+A, +CC[_], +C] extends collection.SeqOps[A, CC, C] {

  /** A copy of the $coll with an element prepended.
   *
   *  @param  elem   the prepended element
   *  @tparam B      the element type of the returned $coll.
   *  @return a new collection of type `Vector[B]` consisting of `value` followed
   *          by all elements of this $coll.
   *
   *    @inheritdoc
   *
   *    Also, the original $coll is not modified, so you will want to capture the result.
   *
   *    Example:
   *    {{{
   *      scala> val x = List(1)
   *      x: List[Int] = List(1)
   *
   *      scala> val y = 2 +: x
   *      y: List[Int] = List(2, 1)
   *
   *      scala> println(x)
   *      List(1)
   *    }}}
   *
   *    @return a new $coll consisting of `value` followed
   *            by all elements of this $coll.
   */
  def prepend[B >: A](elem: B): CC[B] = fromIterable(View.Prepend(elem, coll))

  /** Alias for `prepend`.
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    */
  @`inline` final def +: [B >: A](elem: B): CC[B] = prepend(elem)

  /** A copy of this $coll with an element appended.
   *
   *  @param  elem   the appended element
   *  @tparam B      the element type of the returned $coll.
   *  @return a new collection of type `Vector[B]` consisting of
   *          all elements of this $coll followed by `value`.
   *
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    Example:
   *    {{{
   *       scala> val a = List(1)
   *       a: List[Int] = List(1)
   *
   *       scala> val b = a :+ 2
   *       b: List[Int] = List(1, 2)
   *
   *       scala> println(a)
   *       List(1)
   *    }}}
   *
   *    @return a new $coll consisting of
   *            all elements of this $coll followed by `value`.
   */
  def append[B >: A](elem: B): CC[B] = fromIterable(View.Append(coll, elem))

  /** Alias for `append`
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    */
  @`inline` final def :+ [B >: A](elem: B): CC[B] = append(elem)

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
  def updated[B >: A](index: Int, elem: B): CC[B] = fromIterable(View.Updated(coll, index, elem))

}

object Seq extends IterableFactory.Delegate[Seq](List)

/** Base trait for immutable indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A]
                        with collection.IndexedSeq[A]
                        with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]

object IndexedSeq extends IterableFactoryWithBuilder.Delegate[IndexedSeq](Vector)

/** Base trait for immutable indexed Seq operations */
trait IndexedSeqOps[+A, +CC[X] <: IndexedSeq[X], +C] extends SeqOps[A, CC, C] with collection.IndexedSeqOps[A, CC, C]
