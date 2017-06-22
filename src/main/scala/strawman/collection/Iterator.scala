package strawman.collection

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.{Any, Array, Boolean, Int, None, NoSuchElementException, Nothing, Option, Some, Unit}
import strawman.collection.mutable.ArrayBuffer

/** A core Iterator class
  *
  * @define consumesIterator
  * After calling this method, one should discard the iterator it was called
  * on. Using it is undefined and subject to change.
  */
trait Iterator[+A] extends IterableOnce[A] { self =>
  def hasNext: Boolean
  def next(): A
  def iterator() = this

  /** Tests whether a predicate holds for all values produced by this iterator.
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if the given predicate `p` holds for all values
    *                 produced by this iterator, otherwise `false`.
    *  @note          Reuse: $consumesIterator
    */
  def forall(p: A => Boolean): Boolean = {
    var res = true
    while (res && hasNext) res = p(next())
    res
  }

  /** Tests whether a predicate holds for some of the values produced by this iterator.
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if the given predicate `p` holds for some of the values
    *                 produced by this iterator, otherwise `false`.
    *  @note          Reuse: $consumesIterator
    */
  def exists(p: A => Boolean): Boolean = {
    var res = false
    while (!res && hasNext) res = p(next())
    res
  }

  /** Counts the number of elements in the $coll which satisfy a predicate.
    *
    *  @param p     the predicate  used to test elements.
    *  @return      the number of elements satisfying the predicate `p`.
    */
  def count(p: A => Boolean): Int = {
    var res = 0
    while (hasNext) if (p(next())) res += 1
    res
  }

  /** Finds the first value produced by the iterator satisfying a
    *  predicate, if any.
    *  $mayNotTerminateInf
    *
    *  @param p the predicate used to test values.
    *  @return  an option value containing the first value produced by the iterator that satisfies
    *           predicate `p`, or `None` if none exists.
    *  @note    Reuse: $consumesIterator
    */
  def find(p: A => Boolean): Option[A] = {
    while (hasNext) {
      val a = next()
      if (p(a)) return Some(a)
    }
    None
  }

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    while (hasNext) {
      result = op(result, next())
    }
    result
  }

  def foldRight[B](z: B)(op: (A, B) => B): B =
    if (hasNext) op(next(), foldRight(z)(op)) else z

/** Produces a collection containing cumulative results of applying the
   *  operator going left to right.
   *
   *  $willNotTerminateInf
   *  $orderDependent
   *
   *  @tparam B      the type of the elements in the resulting collection
   *  @param z       the initial value
   *  @param op      the binary operator applied to the intermediate result and the element
   *  @return        iterator with intermediate results
   *  @note          Reuse: $consumesAndProducesIterator
   */
  def scanLeft[B](z: B)(op: (B, A) => B): Iterator[B] = new Iterator[B] {
    // We use an intermediate iterator that iterates through the first element `z`
    // and then that will be modified to iterate through the collection
    private var current: Iterator[B] =
      new Iterator[B] {
        def hasNext: Boolean = true
        def next(): B = {
          // Here we change our self-reference to a new iterator that iterates through `self`
          current = new Iterator[B] {
            private var acc = z
            def next(): B = {
              acc = op(acc, self.next())
              acc
            }
            def hasNext: Boolean = self.hasNext
          }
          z
        }
      }
    def next(): B = current.next()
    def hasNext: Boolean = current.hasNext
  }

  def foreach[U](f: A => U): Unit =
    while (hasNext) f(next())

  def indexWhere(p: A => Boolean): Int = {
    var i = 0
    while (hasNext) {
      if (p(next())) return i
      i += 1
    }
    -1
  }

  def length = {
    var len = 0
    while (hasNext) { len += 1; next() }
    len
  }

  def filter(p: A => Boolean): Iterator[A] = new Iterator[A] {
    private var hd: A = _
    private var hdDefined: Boolean = false

    def hasNext: Boolean = hdDefined || {
      do {
        if (!self.hasNext) return false
        hd = self.next()
      } while (!p(hd))
      hdDefined = true
      true
    }

    def next() =
      if (hasNext) {
        hdDefined = false
        hd
      }
      else Iterator.empty.next()
  }

  def map[B](f: A => B): Iterator[B] = new Iterator[B] {
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = new Iterator[B] {
    private var myCurrent: Iterator[B] = Iterator.empty
    private def current = {
      while (!myCurrent.hasNext && self.hasNext)
        myCurrent = f(self.next()).iterator()
      myCurrent
    }
    def hasNext = current.hasNext
    def next() = current.next()
  }

  def ++[B >: A](xs: IterableOnce[B]): Iterator[B] = new Iterator[B] {
    private var myCurrent: Iterator[B] = self
    private var first = true
    private def current = {
      if (!myCurrent.hasNext && first) {
        myCurrent = xs.iterator()
        first = false
      }
      myCurrent
    }
    def hasNext = current.hasNext
    def next() = current.next()
  }

  def take(n: Int): Iterator[A] = new Iterator[A] {
    private var i = 0
    def hasNext = self.hasNext && i < n
    def next() =
      if (hasNext) {
        i += 1
        self.next()
      }
      else Iterator.empty.next()
  }

  /*
   * Implemented by means of a buffer to keep track of the last n elements during iteration.
   */
  def takeRight(n: Int): Iterator[A] = {
    if (n <= 0) Iterator.empty
    else {
      // Return an iterator that iterates over the elements via a buffer
      new Iterator[A]() {
        private[this] var index = 0
        private[this] var count = 0
        // Use a lazy val for the buffer to make sure initialization is done only if needed and at most once
        private[this] lazy val buffer = {
          // Iterate over all elements while keeping track of the last n
          var buf = ArrayBuffer[A]()
          while (self.hasNext) {
            if (index >= buf.length) buf += self.next()
            else buf(index) = self.next()
            index = if ((index + 1) >= n) 0 else index + 1
            if (count < n) count += 1
          }
          // Adjust the starting index if needed
          index = if (index >= buf.length) 0 else index
          buf
        }
        def hasNext: Boolean = {
          // Force initialization of buffer and return whether there are any elements left in the buffer
          buffer != null && count > 0
        }
        def next(): A = {
          val value = buffer(index)
          index = if (index + 1 >= buffer.length) 0 else index + 1
          count -= 1
          value
        }
      }
    }
  }

  def drop(n: Int): Iterator[A] = {
    var i = 0
    while (i < n && hasNext) {
      next()
      i += 1
    }
    this
  }

  /*
   * Implemented by means of a buffer to keep the last n elements from being returned during iteration.
   */
  def dropRight(n: Int): Iterator[A] = {
    if (n <= 0) self
    else {
      // Return an iterator that returns already buffered elements as it buffers new ones (using a buffer of most n elements)
      new Iterator[A]() {
        private[this] var index = 0
        private[this] lazy val buffer = {
          // Fill the buffer with the first n elements (or fewer if the n is greater than the iterator length)
          val buf = ArrayBuffer[A]()
          while (index < n && self.hasNext) {
            buf += self.next()
            index += 1
          }
          index = 0
          buf
        }

        def hasNext: Boolean = {
          // Force initialization of buffer and don't stop until the iterator is exhausted (without having returned the elements currently in the buffer since those are the ones to drop)
          buffer != null && self.hasNext
        }
        def next(): A = {
          val value = buffer(index)
          buffer(index) = self.next()
          index = if (index + 1 >= buffer.length) 0 else index + 1
          value
        }
      }
    }
  }

  /** Skips longest sequence of elements of this iterator which satisfy given
    *  predicate `p`, and returns an iterator of the remaining elements.
    *
    *  @param p the predicate used to skip elements.
    *  @return  an iterator consisting of the remaining elements
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def dropWhile(p: A => Boolean): Iterator[A] = new Iterator[A] {
    // Magic value: -1 = hasn't dropped, 0 = found first, 1 = defer to parent iterator
    private[this] var status = -1
    // Local buffering to avoid double-wrap with .buffered
    private[this] var fst: A = _
    def hasNext: Boolean =
      if (status == 1) self.hasNext
      else if (status == 0) true
      else {
        while (self.hasNext) {
          val a = self.next()
          if (!p(a)) {
            fst = a
            status = 0
            return true
          }
        }
        status = 1
        false
      }
    def next() =
      if (hasNext) {
        if (status == 1) self.next()
        else {
          status = 1
          fst
        }
      }
      else Iterator.empty.next()
  }

  def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = new Iterator[(A, B)] {
    val thatIterator = that.iterator()
    def hasNext = self.hasNext && thatIterator.hasNext
    def next() = (self.next(), thatIterator.next())
  }

  def sameElements[B >: A](that: IterableOnce[B]): Boolean = {
    val those = that.iterator()
    while (hasNext && those.hasNext)
      if (next() != those.next())
        return false
    // At that point we know that *at least one* iterator has no next element
    // If *both* of them have no elements then the collections are the same
    hasNext == those.hasNext
  }
}

object Iterator {

  val empty: Iterator[Nothing] = new Iterator[Nothing] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next on empty iterator")
  }

  def single[A](a: A): Iterator[A] = new Iterator[A] {
    private var consumed: Boolean = false
    def hasNext = !consumed
    def next() = if (consumed) empty.next() else { consumed = true; a }
  }

  def apply[A](xs: A*): Iterator[A] = new IndexedView[A] {
    val length = xs.length
    def apply(n: Int) = xs(n)
  }.iterator()

}
