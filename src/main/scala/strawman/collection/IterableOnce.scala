package strawman
package collection

trait IterableOnce[+A] {
  def iterator(): Iterator[A]
}