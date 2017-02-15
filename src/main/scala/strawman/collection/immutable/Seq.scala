package strawman.collection.immutable

import strawman.collection

/** Base trait of immutable Seq */
trait Seq[+A] extends collection.Seq[A]
                 with SeqLike[A, Seq]
                 with Iterable[A]

/** Immutable indexed Seq */
trait IndexedSeq[+A]
  extends collection.IndexedSeq[A]
    with Seq[A]

/** Immutable Seq */
trait SeqLike[+A, +C[X] <: Seq[X]]
  extends collection.SeqLike[A, C]
