package strawman.collection

import scala.{Array, Char, Int, StringIndexOutOfBoundsException, AnyVal, throws}
import scala.Predef.String
import strawman.collection.mutable.StringBuilder

import scala.reflect.ClassTag

class StringOps(val s: String)
  extends AnyVal
    with IterableOnce[Char]
    with IndexedSeqOps[Char, immutable.IndexedSeq, String]
    with StrictOptimizedIterableOps[Char, immutable.IndexedSeq, String]
    with ArrayLike[Char] {

  def toIterable = StringView(s)
  override def view: StringView = StringView(s)
  protected[this] def coll: String = s
  def toSeq: Seq[Char] = fromIterable(toIterable)

  protected[this] def fromSpecificIterable(coll: Iterable[Char]): String = {
    val sb = new StringBuilder
    for (ch <- coll) sb += ch
    sb.result()
  }

  def iterableFactory = immutable.IndexedSeq

  protected[this] def newSpecificBuilder() = new StringBuilder

  def length = s.length

  @throws[StringIndexOutOfBoundsException]
  def apply(i: Int) = s.charAt(i)

  override def knownSize = s.length

  override def className = "String"

  /** Overloaded version of `map` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def map(f: Char => Char): String = {
    val sb = new StringBuilder
    for (ch <- s) sb += f(ch)
    sb.result()
  }

  /** Overloaded version of `flatMap` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def flatMap(f: Char => String): String = {
    val sb = new StringBuilder
    for (ch <- s) sb ++= f(ch)
    sb.result()
  }

  /** Overloaded version of `++` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def ++(xs: IterableOnce[Char]): String = {
    val sb = new StringBuilder() ++= s
    for (ch <- xs.iterator()) sb += ch
    sb.result()
  }

  /** Another overloaded version of `++`. */
  def ++(xs: String): String = s + xs
}

case class StringView(s: String) extends IndexedView[Char] {
  def length = s.length
  @throws[StringIndexOutOfBoundsException]
  def apply(n: Int) = s.charAt(n)
  override def className = "StringView"
}

