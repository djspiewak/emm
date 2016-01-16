package emm
package effects

import shims.{Applicative, FlatMap, Functor, Monad, Traverse}
import scala.annotation.implicitNotFound

import properties._

@implicitNotFound("could not lift ${E} into stack ${C}; either ${C} does not contain a constructor of ${E}, or there is no Functor for a constructor of ${E}")
trait Lifter[E, C <: Effects] {
  type Out
  type CC[A] = C#Point[A]

  def apply(e: E): CC[Out]
}

object Lifter {
  type Aux[E, C <: Effects, Out0] = Lifter[E, C] { type Out = Out0 }

  implicit def mid1[F[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F], NN: NonNested[C]): Lifter.Aux[F[A], F |: C, A] = new Lifter[F[A], F |: C] {
    type Out = A

    def apply(fa: F[A]) = NN.pack(F.map(fa) { a => C.point(a) })
  }

  implicit def mid2[F[_, _], F2[_, _], Z, A, C <: Effects](implicit ev: Permute2[F, F2], C: Mapper[C], F: Functor[F2[Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[Z, A], F2[Z, ?] |: C, A] = mid1[F2[Z, ?], A, C]
  implicit def mid3[F[_, _, _], F2[_, _, _], Y, Z, A, C <: Effects](implicit ev: Permute3[F, F2], C: Mapper[C], F: Functor[F2[Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[Y, Z, A], F2[Y, Z, ?] |: C, A] = mid1[F2[Y, Z, ?], A, C]

  implicit def midH1[F[_[_], _], G[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F[G, ?]], NN: NonNested[C]): Lifter.Aux[F[G, A], F[G, ?] |: C, A] = mid1[F[G, ?], A, C]
  implicit def midH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, A, C <: Effects](implicit ev: PermuteH2[F, F2], C: Mapper[C], F: Functor[F2[G, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[G, Z, A], F2[G, Z, ?] |: C, A] = mid1[F2[G, Z, ?], A, C]
  implicit def midH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, A, C <: Effects](implicit ev: PermuteH3[F, F2], C: Mapper[C], F: Functor[F2[G, Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: C, A] = mid1[F2[G, Y, Z, ?], A, C]

  implicit def corecurse1[F[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F], NN: NonNested[C]): Lifter.Aux[E, F |: C, L.Out] = new Lifter[E, F |: C] {
    type Out = L.Out

    def apply(e: E) = NN.pack(F.point(L(e)))
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects](implicit ev: Permute2[F, F2], L: Lifter[E, C], F: Applicative[F2[Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[Z, ?] |: C, L.Out] = corecurse1[F2[Z, ?], E, C]
  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, E, C <: Effects](implicit ev: Permute3[F, F2], L: Lifter[E, C], F: Applicative[F2[Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[Y, Z, ?] |: C, L.Out] = corecurse1[F2[Y, Z, ?], E, C]

  implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F[G, ?]], NN: NonNested[C]): Lifter.Aux[E, F[G, ?] |: C, L.Out] = corecurse1[F[G, ?], E, C]
  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, E, C <: Effects](implicit ev: PermuteH2[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[G, Z, ?] |: C, L.Out] = corecurse1[F2[G, Z, ?], E, C]
  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, E, C <: Effects](implicit ev: PermuteH3[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[G, Y, Z, ?] |: C, L.Out] = corecurse1[F2[G, Y, Z, ?], E, C]
}