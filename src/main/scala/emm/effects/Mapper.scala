package emm
package effects

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}
import scala.annotation.implicitNotFound

import properties._

@implicitNotFound("could not compute a method for mapping over effect stack ${C}; either a member of the stack lacks an Applicative, or its Applicative instance is ambiguous")
trait Mapper[C <: Effects] {
  type CC[A] = C#Point[A]

  def point[A](a: A): CC[A]

  def map[A, B](fa: CC[A])(f: A => B): CC[B]
}

trait MapperLowPriorityImplicits {
  import cats.state.State
  import cats.data.Kleisli

  // we require a Binder[F] because we cannot derive a consistent Applicative from Mapper[F]
  implicit def pivotKleisli[Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], FB: Binder[F], FM: Mapper[F], T: Mapper[T]): Mapper[C] = new Mapper[C] {
    import MapperBinder.monad

    def point[A](a: A): CC[A] = NAP.pack(Kleisli.pure[F#Point, Z, T#Point[A]](T.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NAP.pack(NAP.unpack(fa) map { ta => T.map(ta)(f) })
  }
}

object Mapper extends MapperLowPriorityImplicits {

  implicit def base: Mapper[Base] = new Mapper[Base] {
    def point[A](a: A) = a
    def map[A, B](fa: A)(f: A => B) = f(fa)
  }

  implicit def corecurse1[F[_], C <: Effects](implicit P: Mapper[C], NN: NonNested[C], F: Applicative[F]): Mapper[F |: C] = new Mapper[F |: C] {

    def point[A](a: A) = NN.pack(F.pure(P.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NN.pack(F.map(NN.unpack(fa)) { ca => P.map(ca)(f) })
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[Z, ?]]): Mapper[F2[Z, ?] |: C] = new Mapper[F2[Z, ?] |: C] {

    def point[A](a: A) = NN.pack[F2[Z, ?], A](F.pure(P.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NN.pack[F2[Z, ?], B](F.map(NN.unpack[F2[Z, ?], A](fa)) { ca => P.map(ca)(f) })
  }

  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[Y, Z, ?]]): Mapper[F2[Y, Z, ?] |: C] = new Mapper[F2[Y, Z, ?] |: C] {

    def point[A](a: A) = NN.pack[F2[Y, Z, ?], A](F.pure(P.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NN.pack[F2[Y, Z, ?], B](F.map(NN.unpack[F2[Y, Z, ?], A](fa)) { ca => P.map(ca)(f) })
  }

  implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit P: Mapper[C], NN: NonNested[C], F: Applicative[F[G, ?]]): Mapper[F[G, ?] |: C] = new Mapper[F[G, ?] |: C] {

    def point[A](a: A) = NN.pack[F[G, ?], A](F.pure(P.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NN.pack[F[G, ?], B](F.map(NN.unpack[F[G, ?], A](fa)) { ca => P.map(ca)(f) })
  }

  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[G, Z, ?]]): Mapper[F2[G, Z, ?] |: C] = new Mapper[F2[G, Z, ?] |: C] {

    def point[A](a: A) = NN.pack[F2[G, Z, ?], A](F.pure(P.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NN.pack[F2[G, Z, ?], B](F.map(NN.unpack[F2[G, Z, ?], A](fa)) { ca => P.map(ca)(f) })
  }

  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[G, Y, Z, ?]]): Mapper[F2[G, Y, Z, ?] |: C] = new Mapper[F2[G, Y, Z, ?] |: C] {

    def point[A](a: A) = NN.pack[F2[G, Y, Z, ?], A](F.pure(P.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NN.pack[F2[G, Y, Z, ?], B](F.map(NN.unpack[F2[G, Y, Z, ?], A](fa)) { ca => P.map(ca)(f) })
  }

  implicit def pivot1[Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], Pivot: Applicative[Pivot[F#Point, ?]], T: Mapper[T]): Mapper[C] = new Mapper[C] {

    def point[A](a: A): CC[A] = NAP.pack(Pivot.pure(T.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NAP.pack(Pivot.map(NAP.unpack(fa)) { ta => T.map(ta)(f) })
  }

  implicit def pivot2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot2[?[_], Z, ?], F, T], ev: PermuteH2[Pivot, Pivot2], Pivot: Applicative[Pivot2[F#Point, Z, ?]], T: Mapper[T]): Mapper[C] = new Mapper[C] {

    def point[A](a: A): CC[A] = NAP.pack(Pivot.pure(T.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NAP.pack(Pivot.map(NAP.unpack(fa)) { ta => T.map(ta)(f) })
  }

  implicit def pivot3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot2[?[_], Y, Z, ?], F, T], ev: PermuteH3[Pivot, Pivot2], Pivot: Applicative[Pivot2[F#Point, Y, Z, ?]], T: Mapper[T]): Mapper[C] = new Mapper[C] {

    def point[A](a: A): CC[A] = NAP.pack(Pivot.pure(T.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NAP.pack(Pivot.map(NAP.unpack(fa)) { ta => T.map(ta)(f) })
  }
}
