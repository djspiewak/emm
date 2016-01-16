package emm
package effects

import shims.{Applicative, FlatMap, Functor, Monad, Traverse}
import scala.annotation.implicitNotFound

import properties._

@implicitNotFound("could not prove ${C} is a valid monadic stack; perhaps an effect is lacking a FlatMap, or a non-outer effect is lacking a Traverse")
trait Binder[C <: Effects] {
  type CC[A] = C#Point[A]

  def bind[A, B](cca: CC[A])(f: A => CC[B]): CC[B]
}

object Binder {

  implicit def base: Binder[Base] = new Binder[Base] {
    def bind[A, B](fa: A)(f: A => B): B = f(fa)
  }

  implicit def corecurse1[F[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F], B: FlatMap[F]): Binder[F |: C] = new Binder[F |: C] {

    def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
      val back = B.flatMap(NN.unpack(fca)) { ca =>
        val fcaca = T.traverse(ca) { a => NN.unpack(f(a)) }

        F.map(fcaca) { caca => C.bind(caca) { a => a } }
      }

      NN.pack(back)
    }
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Binder[C], NN: NonNested[C], T: Traverser[C], F: Applicative[F2[Z, ?]], B: FlatMap[F2[Z, ?]]): Binder[F2[Z, ?] |: C] = corecurse1[F2[Z, ?], C]
  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Binder[C], NN: NonNested[C], T: Traverser[C], F: Applicative[F2[Y, Z, ?]], B: FlatMap[F2[Y, Z, ?]]): Binder[F2[Y, Z, ?] |: C] = corecurse1[F2[Y, Z, ?], C]

  implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F[G, ?]], B: FlatMap[F[G, ?]]): Binder[F[G, ?] |: C] = corecurse1[F[G, ?], C]
  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F2[G, Z, ?]], B: FlatMap[F2[G, Z, ?]]): Binder[F2[G, Z, ?] |: C] = corecurse1[F2[G, Z, ?], C]
  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F2[G, Y, Z, ?]], B: FlatMap[F2[G, Y, Z, ?]]): Binder[F2[G, Y, Z, ?] |: C] = corecurse1[F2[G, Y, Z, ?], C]

  implicit def pivot1[Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], Pivot: Monad[Pivot[F#Point, ?]], TB: Binder[T], TT: Traverser[T]): Binder[C] = new Binder[C] {

    def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
      val back: Pivot[F#Point, T#Point[B]] = Pivot.flatMap(NAP.unpack(fca)) { ca =>
        val ptta: Pivot[F#Point, T#Point[T#Point[B]]] = TT.traverse[Pivot[F#Point, ?], A, T#Point[B]](ca)(a => NAP.unpack(f(a)))

        Pivot.map(ptta)(tta => TB.bind(tta)(a => a))
      }

      NAP.pack(back)
    }
  }

  implicit def pivot2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot2[?[_], Z, ?], F, T], ev: PermuteH2[Pivot, Pivot2],  Pivot: Monad[Pivot2[F#Point, Z, ?]], TB: Binder[T], TT: Traverser[T]): Binder[C] = pivot1[Pivot2[?[_], Z, ?], C, F, T]
  implicit def pivot3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot2[?[_], Y, Z, ?], F, T], ev: PermuteH3[Pivot, Pivot2],  Pivot: Monad[Pivot2[F#Point, Y, Z, ?]], TB: Binder[T], TT: Traverser[T]): Binder[C] = pivot1[Pivot2[?[_], Y, Z, ?], C, F, T]
}
