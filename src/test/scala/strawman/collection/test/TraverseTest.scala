package strawman

package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.{ArrayBuffer, Builder, Growable}
import strawman.collection._

import scala.{Any, Either, Int, Left, None, Option, Right, Some, Unit}
import scala.Predef.ArrowAssoc
import scala.math.Ordering
import java.lang.String

class TraverseTest {

  def optionSequence[A, C](xs: Iterable[Option[A]])(factory: FromSpecificIterable[A, C]): Option[C] = {
    def folder[F[X] <: Growable[X]]: (Option[F[A]], Option[A]) => Option[F[A]] = { (bo, xo) =>
      (bo, xo) match {
        case (Some(builder), Some(a)) => Some(builder += a)
        case _ => None
      }
    }
    factory match {
      case iterableBuilder: IterableFactoryWithBuilder[_] =>
        xs.foldLeft[Option[Builder[A, C]]](
          Some(iterableBuilder.newBuilder[A]().asInstanceOf[Builder[A, C]])
        )(
          folder[({ type l[X] = Builder[X, C] })#l]
        ).map(_.result)
      case _ =>
        xs.foldLeft[Option[ArrayBuffer[A]]](Some(new ArrayBuffer[A]))(folder).map(_.to(factory))
    }
  }

  @Test
  def optionSequenceTest: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence(xs1)(immutable.List)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence(xs2)(immutable.TreeSet)
    val o2t: Option[immutable.TreeSet[String]] = o2

    // Breakout-like use case from https://github.com/scala/scala/pull/5233:
    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence(xs4)(immutable.TreeMap)
    val o4t: Option[immutable.TreeMap[Int, String]] = o4
  }

}
