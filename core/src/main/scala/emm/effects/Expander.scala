package emm
package effects

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}
import scala.annotation.implicitNotFound

import properties._

trait Expander[C <: Effects] {
  type CC[_]
  type Out <: Effects

  type Point[A] = C#Point[A]

  def apply[A](fa: Point[A]): Out#Point[CC[A]]
}

trait ExpanderLowPriorityImplicits {
  import cats.state.State

  /*implicit def headState[S]: Expander.Aux[State[S, ?] |: Base, State[S, ?], Base] = new Expander[State[S, ?] |: Base] {
    type CC[A] = State[S, A]
    type Out = Base

    def apply[A](fa: State[S, A]): State[S, A] = fa
  }

  implicit def corecurseState[S, C <: Effects](implicit C: Expander[C]): Expander.Aux[State[S, ?] |: C, C.CC, State[S, ?] |: C.Out] = new Expander[State[S, ?] |: C] {
    type CC[A] = C.CC[A]
    type Out = State[S, ?] |: C.Out

    def apply[A](gca: State[S, C#Point[A]]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }*/
}

object Expander extends ExpanderLowPriorityImplicits {
  type Aux[C <: Effects, CC0[_], Out0 <: Effects] = Expander[C] { type CC[A] = CC0[A]; type Out = Out0 }

  implicit def head1[F[_]]: Expander.Aux[F |: Base, F, Base] = new Expander[F |: Base] {
    type CC[A] = F[A]
    type Out = Base

    def apply[A](fa: F[A]): F[A] = fa
  }

  implicit def head2[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2]): Expander.Aux[F2[Z, ?] |: Base, F2[Z, ?], Base] = new Expander[F2[Z, ?] |: Base] {
    type CC[A] = F2[Z, A]
    type Out = Base

    def apply[A](fa: F2[Z, A]): F2[Z, A] = fa
  }

  implicit def head3[F[_, _, _], F2[_, _, _], Y, Z](implicit ev: Permute3[F, F2]): Expander.Aux[F2[Y, Z, ?] |: Base, F2[Y, Z, ?], Base] = new Expander[F2[Y, Z, ?] |: Base] {
    type CC[A] = F2[Y, Z, A]
    type Out = Base

    def apply[A](fa: F2[Y, Z, A]): F2[Y, Z, A] = fa
  }

  implicit def headH1[F[_[_], _], G[_]]: Expander.Aux[F[G, ?] |: Base, F[G, ?], Base] = new Expander[F[G, ?] |: Base] {
    type CC[A] = F[G, A]
    type Out = Base

    def apply[A](fa: F[G, A]): F[G, A] = fa
  }

  implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z](implicit ev: PermuteH2[F, F2]): Expander.Aux[F2[G, Z, ?] |: Base, F2[G, Z, ?], Base] = new Expander[F2[G, Z, ?] |: Base] {
    type CC[A] = F2[G, Z, A]
    type Out = Base

    def apply[A](fa: F2[G, Z, A]): F2[G, Z, A] = fa
  }

  implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z](implicit ev: PermuteH3[F, F2]): Expander.Aux[F2[G, Y, Z, ?] |: Base, F2[G, Y, Z, ?], Base] = new Expander[F2[G, Y, Z, ?] |: Base] {
    type CC[A] = F2[G, Y, Z, A]
    type Out = Base

    def apply[A](fa: F2[G, Y, Z, A]): F2[G, Y, Z, A] = fa
  }

  implicit def corecurse1[F[_], C <: Effects](implicit C: Expander[C]): Expander.Aux[F |: C, C.CC, F |: C.Out] = new Expander[F |: C] {
    type CC[A] = C.CC[A]
    type Out = F |: C.Out

    def apply[A](gca: Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Expander[C]): Expander.Aux[F2[Z, ?] |: C, C.CC, F2[Z, ?] |: C.Out] = new Expander[F2[Z, ?] |: C] {
    type CC[A] = C.CC[A]
    type Out = F2[Z, ?] |: C.Out

    def apply[A](gca: Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Expander[C]): Expander.Aux[F2[Y, Z, ?] |: C, C.CC, F2[Y, Z, ?] |: C.Out] = new Expander[F2[Y, Z, ?] |: C] {
    type CC[A] = C.CC[A]
    type Out = F2[Y, Z, ?] |: C.Out

    def apply[A](gca: Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: Expander[C]): Expander.Aux[F[G, ?] |: C, C.CC, F[G, ?] |: C.Out] = new Expander[F[G, ?] |: C] {
    type CC[A] = C.CC[A]
    type Out = F[G, ?] |: C.Out

    def apply[A](gca: Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Expander[C]): Expander.Aux[F2[G, Z, ?] |: C, C.CC, F2[G, Z, ?] |: C.Out] = new Expander[F2[G, Z, ?] |: C] {
    type CC[A] = C.CC[A]
    type Out = F2[G, Z, ?] |: C.Out

    def apply[A](gca: Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Expander[C]): Expander.Aux[F2[G, Y, Z, ?] |: C, C.CC, F2[G, Y, Z, ?] |: C.Out] = new Expander[F2[G, Y, Z, ?] |: C] {
    type CC[A] = C.CC[A]
    type Out = F2[G, Y, Z, ?] |: C.Out

    def apply[A](gca: Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def pivot1Base[Pivot[_[_], _], C <: Effects, F <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, Base]): Expander.Aux[C, Pivot[F#Point, ?], Base] = new Expander[C] {
    type CC[A] = Pivot[F#Point, A]
    type Out = Base

    def apply[A](gca: C#Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  // C == F ++ (Pivot -|: T)
  implicit def pivot1[Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], T: Expander[T]): Expander.Aux[C, T.CC, F#Append[Pivot -|: T.Out]] = new Expander[C] {
    type CC[A] = T.CC[A]
    type Out = F#Append[Pivot -|: T.Out]

    def apply[A](gca: C#Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def pivot2Base[Pivot[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Z, ?], F, T]): Expander.Aux[C, Pivot[F#Point, Z, ?], Base] = new Expander[C] {
    type CC[A] = Pivot[F#Point, Z, A]
    type Out = Base

    def apply[A](gca: C#Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def pivot2[Pivot[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Z, ?], F, T], T: Expander[T]): Expander.Aux[C, T.CC, F#Append[Pivot[?[_], Z, ?] -|: T.Out]] = new Expander[C] {
    type CC[A] = T.CC[A]
    type Out = F#Append[Pivot[?[_], Z, ?] -|: T.Out]

    def apply[A](gca: C#Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def pivot3Base[Pivot[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Y, Z, ?], F, T]): Expander.Aux[C, Pivot[F#Point, Y, Z, ?], Base] = new Expander[C] {
    type CC[A] = Pivot[F#Point, Y, Z, A]
    type Out = Base

    def apply[A](gca: C#Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }

  implicit def pivot3[Pivot[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Y, Z, ?], F, T], T: Expander[T]): Expander.Aux[C, T.CC, F#Append[Pivot[?[_], Y, Z, ?] -|: T.Out]] = new Expander[C] {
    type CC[A] = T.CC[A]
    type Out = F#Append[Pivot[?[_], Y, Z, ?] -|: T.Out]

    def apply[A](gca: C#Point[A]): Out#Point[CC[A]] =
      gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
  }
}
