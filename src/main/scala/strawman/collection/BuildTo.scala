package strawman.collection

import strawman.collection.mutable.Builder

/** Auxiliary data type used to retrieve the element type `Elem` of a complete collection type `C` */
trait BuildTo[C] {

  type Elem

  def newBuilder(): Builder[Elem, C]

}

object BuildTo {

  /** Provides a `BuildTo` based on an available `CanBuild` for a unary collection type constructor */
  implicit def unaryTC[CC[_], A](implicit cb: CanBuild[A, CC[A]]): BuildTo[CC[A]] { type Elem = A } =
    new BuildTo[CC[A]] {
      type Elem = A
      def newBuilder(): Builder[A, CC[A]] = cb.newBuilder()
    }

  /** Provides a `BuildTo` based on an available `CanBuild` for a binary collection type constructor */
  implicit def binaryTC[CC[_, _], A, B](implicit cb: CanBuild[(A, B), CC[A, B]]): BuildTo[CC[A, B]] { type Elem = (A, B) } =
    new BuildTo[CC[A, B]] {
      type Elem = (A, B)
      def newBuilder(): Builder[(A, B), CC[A, B]] = cb.newBuilder()
    }

}
