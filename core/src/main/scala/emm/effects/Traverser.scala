package emm
package effects

import shims.{Applicative, FlatMap, Functor, Monad, Traverse}
import scala.annotation.implicitNotFound

import properties._

trait Traverser[C <: Effects] {
  type CC[A] = C#Point[A]

  def traverse[G[_]: Applicative, A, B](ca: CC[A])(f: A => G[B]): G[CC[B]]
}

object Traverser {

  implicit def base: Traverser[Base] = new Traverser[Base] {
    def traverse[G[_]: Applicative, A, B](fa: A)(f: A => G[B]): G[B] = f(fa)
  }

  implicit def corecurse1[F[_], C <: Effects](implicit C: Traverser[C], NN: NonNested[C], F: Traverse[F]): Traverser[F |: C] = new Traverser[F |: C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = F.traverse(NN.unpack(fca)) { ca =>
        C.traverse(ca)(f)
      }

      implicitly[Applicative[G]].map(back) { fca2 => NN.pack(fca2) }
    }
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[Z, ?]]): Traverser[F2[Z, ?] |: C] = corecurse1[F2[Z, ?], C]
  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[Y, Z, ?]]): Traverser[F2[Y, Z, ?] |: C] = corecurse1[F2[Y, Z, ?], C]

  implicit def corecurseH1[F[_[_], _], G0[_], C <: Effects](implicit C: Traverser[C], NN: NonNested[C], F: Traverse[F[G0, ?]]): Traverser[F[G0, ?] |: C] = corecurse1[F[G0, ?], C]
  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G0[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[G0, Z, ?]]): Traverser[F2[G0, Z, ?] |: C] = corecurse1[F2[G0, Z, ?], C]
  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G0[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[G0, Y, Z, ?]]): Traverser[F2[G0, Y, Z, ?] |: C] = corecurse1[F2[G0, Y, Z, ?], C]

  implicit def pivot1[Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], T: Traverser[T], Pivot: Traverse[Pivot[F#Point, ?]]): Traverser[C] = new Traverser[C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = Pivot.traverse(NAP.unpack(fca)) { ca =>
        T.traverse(ca)(f)
      }

      implicitly[Applicative[G]].map(back) { fca2 => NAP.pack(fca2) }
    }
  }

  implicit def pivot2[Pivot[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Z, ?], F, T], T: Traverser[T], Pivot: Traverse[Pivot[F#Point, Z, ?]]): Traverser[C] = pivot1[Pivot[?[_], Z, ?], C, F, T]
  implicit def pivot3[Pivot[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot[?[_], Y, Z, ?], F, T], T: Traverser[T], Pivot: Traverse[Pivot[F#Point, Y, Z, ?]]): Traverser[C] = pivot1[Pivot[?[_], Y, Z, ?], C, F, T]
}