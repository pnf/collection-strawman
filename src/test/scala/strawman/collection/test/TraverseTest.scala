package strawman

package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.Builder
import strawman.collection._

import scala.{Any, Either, Int, Left, None, Option, Right, Some, Unit}
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

class TraverseTest {

  implicitly[BuildFrom[immutable.List[(Int, String)], (Int, String)]]
  implicitly[BuildFrom[immutable.TreeMap[Int, String], (Int, String)]]

  def optionSequence[CC[X] <: Iterable[X], A](xs: CC[Option[A]])(implicit bf: BuildFrom[CC[Option[A]], A]): Option[bf.To] =
    xs.foldLeft[Option[Builder[A, bf.To]]](Some(bf.newBuilder())) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  def eitherSequence[CC[X] <: Iterable[X], A, B](xs: CC[Either[A, B]])(implicit bf: BuildFrom[CC[Either[A, B]], B]): Either[A, bf.To] =
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

}
