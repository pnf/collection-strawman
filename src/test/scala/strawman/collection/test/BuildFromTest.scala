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

/**
  * Auxiliary type allowing us to define which collection type to build according to the type of an existing collection.
  * @tparam From Existing collection type that will drives the collection type to build
  * @tparam A Element type
  */
trait BuildFrom[-From, -A] {

  /** Collection type to build */
  type To

  def newBuilder(): Builder[A, To]

}

trait BuildFromLowPriority {

  /**
    * Extracts the binary type constructor of `From` and applies it to `A` and `B`
    * to build the `To` type.
    */
  implicit def binaryTC[CC[_, _], A, B](implicit
    cb: CanBuild[(A, B), CC[A, B]]
  ): BuildFrom.Aux[CC[_, _], (A, B), CC[A, B]] =
    new BuildFrom[CC[_, _], (A, B)] {
      type To = CC[A, B]
      def newBuilder(): Builder[(A, B), CC[A, B]] = cb.newBuilder()
    }

}

object BuildFrom extends BuildFromLowPriority {

  type Aux[From, A, To0] = BuildFrom[From, A] { type To = To0 }

  /**
    * Extracts the unary type constructor of `From` and applies it to `A`
    * to build the `To` type.
    */
  implicit def unaryTC[CC[_], A](implicit
    cb: CanBuild[A, CC[A]]
  ): BuildFrom.Aux[CC[_], A, CC[A]] =
    new BuildFrom[CC[_], A] {
      type To = CC[A]
      def newBuilder(): Builder[A, CC[A]] = cb.newBuilder()
    }

  // Explicit BuildFrom instances allowing breakOut-like style (see below in tests for usage examples)
  // We have to define four variants to support all the combinations of factories (unary vs binary type
  // constructor, ordered vs unordered)

  def factory[CC[_], A](iterableFactory: IterableFactory[CC]): BuildFrom.Aux[Any, A, CC[A]] =
    new BuildFrom[Any, A] {
      type To = CC[A]
      def newBuilder(): Builder[A, CC[A]] = iterableFactory.newBuilder[A]
    }

  def factory[CC[_], A](orderedIterableFactory: OrderedIterableFactory[CC])(implicit ordering: Ordering[A]): BuildFrom.Aux[Any, A, CC[A]] =
    new BuildFrom[Any, A] {
      type To = CC[A]
      def newBuilder(): Builder[A, CC[A]] = orderedIterableFactory.newBuilder[A]
    }

  def factory[CC[X, Y] <: Map[X, Y] with MapOps[X, Y, CC, _], K, V](
    mapFactory: MapFactory[CC]
  ): BuildFrom.Aux[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V)] {
      type To = CC[K, V]
      def newBuilder(): Builder[(K, V), CC[K, V]] = mapFactory.newBuilder[K, V]
    }

  def factory[CC[X, Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], K, V](
    orderedMapFactory: OrderedMapFactory[CC]
  )(implicit
    ordering: Ordering[K]
  ): BuildFrom.Aux[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V)] {
      type To = CC[K, V]
      def newBuilder(): Builder[(K, V), CC[K, V]] = orderedMapFactory.newBuilder[K, V]
    }

}

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

//  @Test
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
    val xs3t: mutable.ListBuffer[Either[Int, String]] = xs3
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


//  @Test
  def flatCollectTest: Unit = {
    val xs1 = immutable.List(1, 2, 3)
    val xs2 = flatCollect(xs1) { case 2 => mutable.ArrayBuffer("foo", "bar") }
    val xs3: immutable.List[String] = xs2

    val xs4 = immutable.TreeMap((1, "1"), (2, "2"))
    val xs5 = flatCollect(xs4) { case (2, v) => immutable.List((v, v)) }
    val xs6: immutable.TreeMap[String, String] = xs5
  }

//  @Test
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
