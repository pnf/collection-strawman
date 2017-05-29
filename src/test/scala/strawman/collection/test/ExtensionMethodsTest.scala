package strawman

package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.{ArrayBuffer, Builder}
import strawman.collection._

import scala.{Any, Boolean, Either, Int, Left, None, Option, PartialFunction, Right, Some, Unit}
import scala.Predef.{ArrowAssoc, implicitly}
import scala.math.Ordering
import java.lang.String

class ExtensionMethodsTest {

  // From https://github.com/scala/collection-strawman/issues/44
  def flatCollect[A, B, C](coll: Iterable[A])(f: PartialFunction[A, IterableOnce[B]])(factory: FromSpecificIterable[B, C]): C = {
    val buffer = new ArrayBuffer[B]()
    for (a <- coll) {
      if (f.isDefinedAt(a)) {
        buffer ++= f(a)
      }
    }
    factory.fromSpecificIterable(buffer)
  }

  def mapSplit[A, B, C, C1, C2](coll: Iterable[A])(f: A => Either[B, C])(leftFactory: FromSpecificIterable[B, C1], rightFactory: FromSpecificIterable[C, C2]): (C1, C2) = {
    val left = new ArrayBuffer[B]()
    val right = new ArrayBuffer[C]()
    for (a <- coll) {
      f(a).fold(left.add, right.add)
    }
    (leftFactory.fromSpecificIterable(left), rightFactory.fromSpecificIterable(right))
  }


  @Test
  def flatCollectTest: Unit = {
    val xs1 = immutable.List(1, 2, 3)
    val xs2 = flatCollect(xs1) { case 2 => mutable.ArrayBuffer("foo", "bar") }(immutable.List)
    val xs3: immutable.List[String] = xs2

    val xs4 = immutable.TreeMap((1, "1"), (2, "2"))
    val xs5 = flatCollect(xs4) { case (2, v) => immutable.List((v, v)) }(immutable.TreeMap)
    val xs6: immutable.TreeMap[String, String] = xs5
  }

  @Test
  def mapSplitTest: Unit = {
    val xs1 = immutable.List(1, 2, 3)
    val (xs2, xs3) = mapSplit(xs1)(x => if (x % 2 == 0) Left(x) else Right(x.toString))(immutable.List, immutable.List)
    val xs4: immutable.List[Int] = xs2
    val xs5: immutable.List[String] = xs3

    val xs6 = immutable.TreeMap((1, "1"), (2, "2"))
    val (xs7, xs8) = mapSplit(xs6) { case (k, v) => Left[(String, Int), (Int, Boolean)]((v, k)) }(immutable.TreeMap, immutable.TreeMap)
    val xs9: immutable.TreeMap[String, Int] = xs7
    val xs10: immutable.TreeMap[Int, Boolean] = xs8
  }

}