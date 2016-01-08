package emm
package effects

import cats.{~>, Applicative, FlatMap, Functor, Monad, Traverse, Eval}
import scala.annotation.implicitNotFound

import properties._

@implicitNotFound("could not lift ${E} into stack ${C}; either ${C} does not contain a constructor of ${E}, or there is no Functor for a constructor of ${E}")
trait Lifter[E, C <: Effects] {
  type Out
  type CC[A] = C#Point[A]

  def apply(e: E): CC[Out]
}

trait LifterLowPriorityImplicits {
  import cats.data.Kleisli

  implicit def midKleisli[A, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], F: Mapper[F], T: Mapper[T]): Lifter.Aux[Kleisli[λ[X => X], Z, A], C, A] = new Lifter[Kleisli[λ[X => X], Z, A], C] {
    type Out = A

    def apply(e: Kleisli[λ[X => X], Z, A]): CC[Out] = {
      val t = new (λ[X => X] ~> F#Point) {
        def apply[A](a: A): F#Point[A] = F.point(a)
      }

      NAP.pack(e.transform[F#Point](t).map(T.point)(F.functor))
    }
  }

  implicit def leftPivotKleisli[E, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], T: Mapper[T], F: Lifter[E, F], FM: Mapper[F]): Lifter.Aux[E, C, F.Out] = new Lifter[E, C] {
    type Out = F.Out

    def apply(e: E): CC[Out] =
      NAP.pack(Kleisli[F#Point, Z, T#Point[F.Out]]({ _ => FM.map(F(e)) { a => T.point(a) } }))
  }

  implicit def rightPivotKleisli[E, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], T: Lifter[E, T], F: Mapper[F]): Lifter.Aux[E, C, T.Out] = new Lifter[E, C] {
    type Out = T.Out

    def apply(e: E): CC[Out] = NAP.pack(Kleisli[F#Point, Z, T#Point[T.Out]] { _ => F.point(T(e)) })
  }
}

object Lifter extends LifterLowPriorityImplicits {
  type Aux[E, C <: Effects, Out0] = Lifter[E, C] { type Out = Out0 }

  implicit def mid1[F[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F], NN: NonNested[C]): Lifter.Aux[F[A], F |: C, A] = new Lifter[F[A], F |: C] {
    type Out = A

    def apply(fa: F[A]) = NN.pack(F.map(fa) { a => C.point(a) })
  }

  implicit def mid2[F[_, _], F2[_, _], Z, A, C <: Effects](implicit ev: Permute2[F, F2], C: Mapper[C], F: Functor[F2[Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[Z, A], F2[Z, ?] |: C, A] = new Lifter[F2[Z, A], F2[Z, ?] |: C] {
    type Out = A

    def apply(fa: F2[Z, A]) = NN.pack[F2[Z, ?], A](F.map(fa) { a => C.point(a) })
  }

  implicit def mid3[F[_, _, _], F2[_, _, _], Y, Z, A, C <: Effects](implicit ev: Permute3[F, F2], C: Mapper[C], F: Functor[F2[Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[Y, Z, A], F2[Y, Z, ?] |: C, A] = new Lifter[F2[Y, Z, A], F2[Y, Z, ?] |: C] {
    type Out = A

    def apply(fa: F2[Y, Z, A]) = NN.pack[F2[Y, Z, ?], A](F.map(fa) { a => C.point(a) })
  }

  implicit def midH1[F[_[_], _], G[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F[G, ?]], NN: NonNested[C]): Lifter.Aux[F[G, A], F[G, ?] |: C, A] = new Lifter[F[G, A], F[G, ?] |: C] {
    type Out = A

    def apply(fa: F[G, A]) = NN.pack[F[G, ?], A](F.map((fa)) { a => C.point(a) })
  }

  implicit def midH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, A, C <: Effects](implicit ev: PermuteH2[F, F2], C: Mapper[C], F: Functor[F2[G, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[G, Z, A], F2[G, Z, ?] |: C, A] = new Lifter[F2[G, Z, A], F2[G, Z, ?] |: C] {
    type Out = A

    def apply(fa: F2[G, Z, A]) = NN.pack[F2[G, Z, ?], A](F.map(fa) { a => C.point(a) })
  }

  implicit def midH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, A, C <: Effects](implicit ev: PermuteH3[F, F2], C: Mapper[C], F: Functor[F2[G, Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: C, A] = new Lifter[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: C] {
    type Out = A

    def apply(fa: F2[G, Y, Z, A]) = NN.pack[F2[G, Y, Z, ?], A](F.map(fa) { a => C.point(a) })
  }

  implicit def corecurse1[F[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F], NN: NonNested[C]): Lifter.Aux[E, F |: C, L.Out] = new Lifter[E, F |: C] {
    type Out = L.Out

    def apply(e: E) = NN.pack(F.pure(L(e)))
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects](implicit ev: Permute2[F, F2], L: Lifter[E, C], F: Applicative[F2[Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[Z, ?] |: C, L.Out] = new Lifter[E, F2[Z, ?] |: C] {
    type Out = L.Out

    def apply(e: E) = NN.pack[F2[Z, ?], Out](F.pure(L(e)))
  }

  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, E, C <: Effects](implicit ev: Permute3[F, F2], L: Lifter[E, C], F: Applicative[F2[Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[Y, Z, ?] |: C, L.Out] = new Lifter[E, F2[Y, Z, ?] |: C] {
    type Out = L.Out

    def apply(e: E) = NN.pack[F2[Y, Z, ?], Out](F.pure(L(e)))
  }

  implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F[G, ?]], NN: NonNested[C]): Lifter.Aux[E, F[G, ?] |: C, L.Out] = new Lifter[E, F[G, ?] |: C] {
    type Out = L.Out

    def apply(e: E) = NN.pack[F[G, ?], Out](F.pure(L(e)))
  }

  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, E, C <: Effects](implicit ev: PermuteH2[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[G, Z, ?] |: C, L.Out] = new Lifter[E, F2[G, Z, ?] |: C] {
    type Out = L.Out

    def apply(e: E) = NN.pack[F2[G, Z, ?], Out](F.pure(L(e)))
  }

  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, E, C <: Effects](implicit ev: PermuteH3[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[G, Y, Z, ?] |: C, L.Out] = new Lifter[E, F2[G, Y, Z, ?] |: C] {
    type Out = L.Out

    def apply(e: E) = NN.pack[F2[G, Y, Z, ?], Out](F.pure(L(e)))
  }
}