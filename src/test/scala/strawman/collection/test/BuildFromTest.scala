package strawman
package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.Builder
import strawman.collection._

import scala.{Any, Boolean, Either, Int, Left, None, Option, PartialFunction, Right, Some, Unit}
import scala.Predef.{ArrowAssoc, implicitly}
import scala.math.Ordering
import java.lang.String

class BuildFromTest {

  implicitly[BuildFrom[immutable.List[(Int, String)], (Int, String)]]
  implicitly[BuildFrom[immutable.TreeMap[Int, String], (Int, String)]]

  def optionSequence[A](xs: Iterable[Option[A]])(implicit bf: BuildFrom[xs.type, A]): Option[bf.To] =
    xs.foldLeft[Option[Builder[A, bf.To]]](Some(bf.newBuilder())) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  def eitherSequence[A, B](xs: Iterable[Either[A, B]])(implicit bf: BuildFrom[xs.type, B]): Either[A, bf.To] =
    xs.foldLeft[Either[A, Builder[B, bf.To]]](Right(bf.newBuilder())) {
      case (Right(builder), Right(b)) => Right(builder += b)
      case (Left(a)       ,        _) => Left(a)
      case (_             ,  Left(a)) => Left(a)
    }.right.map(_.result)

  @Test
  def optionSequenceTest: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence(xs2)
    val o2t: Option[immutable.TreeSet[String]] = o2

    // Breakout-like use case from https://github.com/scala/scala/pull/5233:
    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence(xs4)(BuildFrom.factory(immutable.TreeMap))
    val o4t: Option[immutable.TreeMap[Int, String]] = o4
  }

  @Test
  def eitherSequenceTest: Unit = {
    val xs3 = mutable.ListBuffer(Right("foo"), Left(0), Right("bar"))
    val xs3t: mutable.ListBuffer[Either[Int, String] with scala.Product with scala.Serializable] = xs3 // TODO Remove `with Product with Serializable` when dotty uses Scala 2.12 library
    val e1 = eitherSequence(xs3)
    val e1t: Either[Int, mutable.ListBuffer[String]] = e1
  }

  // From https://github.com/scala/collection-strawman/issues/44
  def flatCollect[A, B](
    coll: Iterable[A]
  )(
    f: PartialFunction[A, IterableOnce[B]]
  )(implicit
    bf: BuildFrom[coll.type, B]
  ): bf.To = {
    val builder = bf.newBuilder()
    for (a <- coll) {
      if (f.isDefinedAt(a)) {
        builder ++= f(a)
      }
    }
    builder.result
  }

  def mapSplit[A, B, C](
    coll: Iterable[A]
  )(
    f: A => Either[B, C]
  )(implicit
    bfLeft:  BuildFrom[coll.type, B],
    bfRight: BuildFrom[coll.type, C]
  ): (bfLeft.To, bfRight.To) = {
    val left = bfLeft.newBuilder()
    val right = bfRight.newBuilder()

    for (a <- coll) {
      f(a).fold(left.add, right.add)
    }

    (left.result, right.result)
  }


  @Test
  def flatCollectTest: Unit = {
    val xs1 = immutable.List(1, 2, 3)
    val xs2 = flatCollect(xs1) { case 2 => mutable.ArrayBuffer("foo", "bar") }
    val xs3: immutable.List[String] = xs2

    val xs4 = immutable.TreeMap((1, "1"), (2, "2"))
    val xs5 = flatCollect(xs4) { case (2, v) => immutable.List((v, v)) }
    val xs6: immutable.TreeMap[String, String] = xs5
  }

  @Test
  def mapSplitTest: Unit = {
    val xs1 = immutable.List(1, 2, 3)
    val (xs2, xs3) = mapSplit(xs1)(x => if (x % 2 == 0) Left(x) else Right(x.toString))
    val xs4: immutable.List[Int] = xs2
    val xs5: immutable.List[String] = xs3

    val xs6 = immutable.TreeMap((1, "1"), (2, "2"))
    val (xs7, xs8) = mapSplit(xs6) { case (k, v) => Left[(String, Int), (Int, Boolean)]((v, k)) }
    val xs9: immutable.TreeMap[String, Int] = xs7
    val xs10: immutable.TreeMap[Int, Boolean] = xs8
  }

}