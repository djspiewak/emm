package emm
package effects

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}
import scala.annotation.implicitNotFound

import properties._

trait Traverser[C <: Effects] {
  type CC[A] = C#Point[A]

  def traverse[G[_]: Applicative, A, B](ca: CC[A])(f: A => G[B]): G[CC[B]]
  def foldLeft[A, B](fa: CC[A], b: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
}

object Traverser {

  implicit def base: Traverser[Base] = new Traverser[Base] {
    def traverse[G[_]: Applicative, A, B](fa: A)(f: A => G[B]): G[B] = f(fa)
    def foldLeft[A, B](fa: A, b: B)(f: (B, A) => B) = f(b, fa)
    def foldRight[A, B](fa: A, b: Eval[B])(f: (A, Eval[B]) => Eval[B]) = f(fa, b)
  }

  implicit def corecurse1[F[_], C <: Effects](implicit C: Traverser[C], NN: NonNested[C], F: Traverse[F]): Traverser[F |: C] = new Traverser[F |: C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = F.traverse(NN.unpack(fca)) { ca =>
        C.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NN.pack(fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      F.foldLeft(NN.unpack(fca), b) { (b, ca) =>
        C.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      F.foldRight(NN.unpack(fca), lb) { (ca, eb) =>
        C.foldRight(ca, eb)(f)
      }
    }

  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[Z, ?]]): Traverser[F2[Z, ?] |: C] = new Traverser[F2[Z, ?] |: C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = F.traverse(NN.unpack[F2[Z, ?], A](fca)) { ca =>
        C.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NN.pack[F2[Z, ?], B](fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      F.foldLeft(NN.unpack[F2[Z, ?], A](fca), b) { (b, ca) =>
        C.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      F.foldRight(NN.unpack[F2[Z, ?], A](fca), lb) { (ca, eb) =>
        C.foldRight(ca, eb)(f)
      }
    }
  }

  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[Y, Z, ?]]): Traverser[F2[Y, Z, ?] |: C] = new Traverser[F2[Y, Z, ?] |: C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = F.traverse(NN.unpack[F2[Y, Z, ?], A](fca)) { ca =>
        C.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NN.pack[F2[Y, Z, ?], B](fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      F.foldLeft(NN.unpack[F2[Y, Z, ?], A](fca), b) { (b, ca) =>
        C.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      F.foldRight(NN.unpack[F2[Y, Z, ?], A](fca), lb) { (ca, eb) =>
        C.foldRight(ca, eb)(f)
      }
    }
  }

  implicit def corecurseH1[F[_[_], _], G0[_], C <: Effects](implicit C: Traverser[C], NN: NonNested[C], F: Traverse[F[G0, ?]]): Traverser[F[G0, ?] |: C] = new Traverser[F[G0, ?] |: C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = F.traverse(NN.unpack[F[G0, ?], A](fca)) { ca =>
        C.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NN.pack[F[G0, ?], B](fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      F.foldLeft(NN.unpack[F[G0, ?], A](fca), b) { (b, ca) =>
        C.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      F.foldRight(NN.unpack[F[G0, ?], A](fca), lb) { (ca, eb) =>
        C.foldRight(ca, eb)(f)
      }
    }
  }

  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G0[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[G0, Z, ?]]): Traverser[F2[G0, Z, ?] |: C] = new Traverser[F2[G0, Z, ?] |: C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = F.traverse(NN.unpack[F2[G0, Z, ?], A](fca)) { ca =>
        C.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NN.pack[F2[G0, Z, ?], B](fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      F.foldLeft(NN.unpack[F2[G0, Z, ?], A](fca), b) { (b, ca) =>
        C.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      F.foldRight(NN.unpack[F2[G0, Z, ?], A](fca), lb) { (ca, eb) =>
        C.foldRight(ca, eb)(f)
      }
    }
  }

  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G0[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[G0, Y, Z, ?]]): Traverser[F2[G0, Y, Z, ?] |: C] = new Traverser[F2[G0, Y, Z, ?] |: C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = F.traverse(NN.unpack[F2[G0, Y, Z, ?], A](fca)) { ca =>
        C.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NN.pack[F2[G0, Y, Z, ?], B](fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      F.foldLeft(NN.unpack[F2[G0, Y, Z, ?], A](fca), b) { (b, ca) =>
        C.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      F.foldRight(NN.unpack[F2[G0, Y, Z, ?], A](fca), lb) { (ca, eb) =>
        C.foldRight(ca, eb)(f)
      }
    }
  }

  implicit def pivot1[Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], T: Traverser[T], Pivot: Traverse[Pivot[F#Point, ?]]): Traverser[C] = new Traverser[C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = Pivot.traverse(NAP.unpack(fca)) { ca =>
        T.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NAP.pack(fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      Pivot.foldLeft(NAP.unpack(fca), b) { (b, ca) =>
        T.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      Pivot.foldRight(NAP.unpack(fca), lb) { (ca, eb) =>
        T.foldRight(ca, eb)(f)
      }
    }
  }

  implicit def pivot2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[Pivot, Pivot2], NAP: NestedAtPoint[C, Pivot2[?[_], Z, ?], F, T], T: Traverser[T], Pivot: Traverse[Pivot2[F#Point, Z, ?]]): Traverser[C] = new Traverser[C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = Pivot.traverse(NAP.unpack(fca)) { ca =>
        T.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NAP.pack(fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      Pivot.foldLeft(NAP.unpack(fca), b) { (b, ca) =>
        T.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      Pivot.foldRight(NAP.unpack(fca), lb) { (ca, eb) =>
        T.foldRight(ca, eb)(f)
      }
    }
  }

  implicit def pivot3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[Pivot, Pivot2], NAP: NestedAtPoint[C, Pivot2[?[_], Y, Z, ?], F, T], T: Traverser[T], Pivot: Traverse[Pivot2[F#Point, Y, Z, ?]]): Traverser[C] = new Traverser[C] {

    def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
      val back = Pivot.traverse(NAP.unpack(fca)) { ca =>
        T.traverse(ca)(f)
      }

      Applicative[G].map(back) { fca2 => NAP.pack(fca2) }
    }

    def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
      Pivot.foldLeft(NAP.unpack(fca), b) { (b, ca) =>
        T.foldLeft(ca, b)(f)
      }
    }

    def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      Pivot.foldRight(NAP.unpack(fca), lb) { (ca, eb) =>
        T.foldRight(ca, eb)(f)
      }
    }
  }
}