package strawman
package collection
package test

import strawman.collection.mutable.Builder

import scala.{Int, None, Option, Some, Unit}
import scala.Predef.implicitly
import java.lang.String

import org.junit.Test

/**
  * Builds a collection of type `C`.
  *
  * @tparam C Type of the collection to produce (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait Unfolder[C] {

  val bt: BuildTo[C]

  def apply[S](init: S)(f: S => Option[(bt.Elem, S)]) = {
    val builder = bt.newBuilder()
    var state = init
    def loop(): Unit = {
      f(state) match {
        case Some((elem, newState)) =>
          state = newState
          builder += elem
          loop()
        case None => ()
      }
    }
    builder.result
  }

}

object Unfold {

  /**
    * @return An `Unfolder` instance for the target type `C`.
    * @tparam C The collection type to build (e.g. `List[Int]`, `TreeMap[Int, String]`)
    */
  def apply[C](implicit _bt: BuildTo[C]): Unfolder[C] { val bt: _bt.type } =
    new Unfolder[C] { val bt: _bt.type = _bt }

}

class UnfoldTest {

  @Test
  def unfoldTest: Unit = {
    implicitly[BuildTo[immutable.List[Int]]]
    implicitly[BuildTo[immutable.TreeMap[Int, String]]]

    val xs1 = Unfold[immutable.List[Int]].apply(0)(n => if (n < 10) Some((n, n + 1)) else None)
    val xs2: immutable.List[Int] = xs1

    val xs3 = Unfold[immutable.TreeSet[Int]].apply(0)(n => if (n < 10) Some((n, n + 1)) else None)
    val xs4: immutable.TreeSet[Int] = xs3

    val xs5 = Unfold[immutable.HashMap[Int, String]].apply(0)(_ => None)
    val xs6: immutable.HashMap[Int, String] = xs5

    val xs7 = Unfold[immutable.TreeMap[Int, String]].apply(0)(_ => None)
    val xs8: immutable.TreeMap[Int, String] = xs7
  }

}