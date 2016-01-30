package emm
package effects

import shims.{Applicative, FlatMap, Functor, Monad, Traverse}
import scala.annotation.implicitNotFound

import properties._

trait Collapser[E, C <: Effects] {
  type A
  type Out <: Effects

  type Point[A] = C#Point[A]

  def apply(fa: Point[E]): Out#Point[A]
}

trait CollapserLowPriorityImplicits2 {

  def head1[F[_], A0]: Collapser.Aux[F[A0], Base, A0, F |: Base]

  implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, A0](implicit ev: PermuteH2[F, F2]): Collapser.Aux[F2[G, Z, A0], Base, A0, F2[G, Z, ?] |: Base] =  head1[F2[G, Z, ?], A0]
  implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, A0](implicit ev: PermuteH3[F, F2]): Collapser.Aux[F2[G, Y, Z, A0], Base, A0, F2[G, Y, Z, ?] |: Base] =  head1[F2[G, Y, Z, ?], A0]

  def corecurse1[E, F[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F |: C, C.A, F |: C.Out]

  implicit def corecurseH2[E, F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[G, Z, ?] |: C, C.A, F2[G, Z, ?] |: C.Out] = corecurse1[E, F2[G, Z, ?], C]
  implicit def corecurseH3[E, F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[G, Y, Z, ?] |: C, C.A, F2[G, Y, Z, ?] |: C.Out] = corecurse1[E, F2[G, Y, Z, ?], C]
}

object Collapser extends CollapserLowPriorityImplicits2 {
  type Aux[E, C <: Effects, A0, Out0 <: Effects] = Collapser[E, C] { type A = A0; type Out = Out0 }

  implicit def head1[F[_], A0]: Collapser.Aux[F[A0], Base, A0, F |: Base] = new Collapser[F[A0], Base] {
    type A = A0
    type Out = F |: Base

    def apply(fa: F[A]): F[A] = fa
  }

  implicit def head2[F[_, _], F2[_, _], Z, A0](implicit ev: Permute2[F, F2]): Collapser.Aux[F2[Z, A0], Base, A0, F2[Z, ?] |: Base] = head1[F2[Z, ?], A0]
  implicit def head3[F[_, _, _], F2[_, _, _], Y, Z, A0](implicit ev: Permute3[F, F2]): Collapser.Aux[F2[Y, Z, A0], Base, A0, F2[Y, Z, ?] |: Base] =  head1[F2[Y, Z, ?], A0]

  implicit def headH1[F[_[_], _], G[_], A0]: Collapser.Aux[F[G, A0], Base, A0, F[G, ?] |: Base] =  head1[F[G, ?], A0]

  implicit def corecurse1[E, F[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F |: C, C.A, F |: C.Out] = new Collapser[E, F |: C] {
    type A = C.A
    type Out = F |: C.Out

    def apply(gca: Point[E]): Out#Point[A] =
      gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
  }

  implicit def corecurse2[E, F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[Z, ?] |: C, C.A, F2[Z, ?] |: C.Out] = corecurse1[E, F2[Z, ?], C]
  implicit def corecurse3[E, F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[Y, Z, ?] |: C, C.A, F2[Y, Z, ?] |: C.Out] = corecurse1[E, F2[Y, Z, ?], C]

  implicit def corecurseH1[E, F[_[_], _], G[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F[G, ?] |: C, C.A, F[G, ?] |: C.Out] = corecurse1[E, F[G, ?], C]

  // C == F ++ (Pivot -|: T)
  implicit def pivot1[E, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], T: Collapser[E, T]): Collapser.Aux[E, C, T.A, F#Append[Pivot -|: T.Out]] = new Collapser[E, C] {
    type A = T.A
    type Out = F#Append[Pivot -|: T.Out]

    def apply(gca: Point[E]): Out#Point[A] =
      gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
  }

  implicit def pivot2[E, Pivot[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Z, ?], F, T], T: Collapser[E, T]): Collapser.Aux[E, C, T.A, F#Append[Pivot[?[_], Z, ?] -|: T.Out]] = pivot1[E, Pivot[?[_], Z, ?], C, F, T]
  implicit def pivot3[E, Pivot[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Y, Z, ?], F, T], T: Collapser[E, T]): Collapser.Aux[E, C, T.A, F#Append[Pivot[?[_], Y, Z, ?] -|: T.Out]] = pivot1[E, Pivot[?[_], Y, Z, ?], C, F, T]
}
