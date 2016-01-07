package emm
package properties

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}
import scala.annotation.implicitNotFound

/**
 * The property of Effects which contain at least one -|: case, partitioning into a front and tail, where the tail
 * is NonNested and the front is unconstrained.
 */
sealed trait NestedAtPoint[C <: Effects, Pivot[_[_], _], Front <: Effects, Tail <: Effects] {
  def NN: NonNested[Tail]

  def pack[A](cc: Pivot[Front#Point, Tail#Point[A]]): C#Point[A]
  def unpack[A](cc: C#Point[A]): Pivot[Front#Point, Tail#Point[A]]
}

object NestedAtPoint {

  implicit def split1[Pivot[_[_], _], C <: Effects](implicit C: NonNested[C]): NestedAtPoint[Pivot -|: C, Pivot, Base, C] = new NestedAtPoint[Pivot -|: C, Pivot, Base, C] {
    def NN = C

    def pack[A](cc: Pivot[λ[X => X], C#Point[A]]): (Pivot -|: C)#Point[A] = cc.asInstanceOf[(Pivot -|: C)#Point[A]]
    def unpack[A](cc: (Pivot -|: C)#Point[A]): Pivot[λ[X => X], C#Point[A]] = cc.asInstanceOf[Pivot[λ[X => X], C#Point[A]]]
  }

  implicit def split2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects](implicit ev: PermuteH2[Pivot, Pivot2], C: NonNested[C]): NestedAtPoint[Pivot2[?[_], Z, ?] -|: C, Pivot2[?[_], Z, ?], Base, C] = new NestedAtPoint[Pivot2[?[_], Z, ?] -|: C, Pivot2[?[_], Z, ?], Base, C] {
    def NN = C

    def pack[A](cc: Pivot2[λ[X => X], Z, C#Point[A]]): (Pivot2[?[_], Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(Pivot2[?[_], Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (Pivot2[?[_], Z, ?] -|: C)#Point[A]): Pivot2[λ[X => X], Z, C#Point[A]] = cc.asInstanceOf[Pivot2[λ[X => X], Z, C#Point[A]]]
  }

  implicit def split3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects](implicit ev: PermuteH3[Pivot, Pivot2], C: NonNested[C]): NestedAtPoint[Pivot2[?[_], Y, Z, ?] -|: C, Pivot2[?[_], Y, Z, ?], Base, C] = new NestedAtPoint[Pivot2[?[_], Y, Z, ?] -|: C, Pivot2[?[_], Y, Z, ?], Base, C] {
    def NN = C

    def pack[A](cc: Pivot2[λ[X => X], Y, Z, C#Point[A]]): (Pivot2[?[_], Y, Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(Pivot2[?[_], Y, Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (Pivot2[?[_], Y, Z, ?] -|: C)#Point[A]): Pivot2[λ[X => X], Y, Z, C#Point[A]] = cc.asInstanceOf[Pivot2[λ[X => X], Y, Z, C#Point[A]]]
  }

  // TODO do I have to multiply all of these cases by various Pivot permutations?  need to test (e.g. StateT)
  implicit def corecurseBar11[G[_], Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G |: C, Pivot, G |: F, T] = new NestedAtPoint[G |: C, Pivot, G |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G |: F)#Point, T#Point[A]]): (G |: C)#Point[A] = cc.asInstanceOf[(G |: C)#Point[A]]
    def unpack[A](cc: (G |: C)#Point[A]): Pivot[(G |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G |: F)#Point, T#Point[A]]]
  }

  implicit def corecurseBar12[G[_], Pivot[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot[?[_], Z, ?], F, T]): NestedAtPoint[G |: C, Pivot[?[_], Z, ?], G |: F, T] = new NestedAtPoint[G |: C, Pivot[?[_], Z, ?], G |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G |: F)#Point, Z, T#Point[A]]): (G |: C)#Point[A] = cc.asInstanceOf[(G |: C)#Point[A]]
    def unpack[A](cc: (G |: C)#Point[A]): Pivot[(G |: F)#Point, Z, T#Point[A]] = cc.asInstanceOf[Pivot[(G |: F)#Point, Z, T#Point[A]]]
  }

  implicit def corecurseBar21[G[_, _], G2[_, _], Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: Permute2[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[Z, ?] |: C, Pivot, G2[Z, ?] |: F, T] = new NestedAtPoint[G2[Z, ?] |: C, Pivot, G2[Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[Z, ?] |: F)#Point, T#Point[A]]): (G2[Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[Z, ?] |: C)#Point[A]): Pivot[(G2[Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[Z, ?] |: F)#Point, T#Point[A]]]
  }

  implicit def corecurseBar31[G[_, _, _], G2[_, _, _], Y, Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: Permute3[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[Y, Z, ?] |: C, Pivot, G2[Y, Z, ?] |: F, T] = new NestedAtPoint[G2[Y, Z, ?] |: C, Pivot, G2[Y, Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[Y, Z, ?] |: F)#Point, T#Point[A]]): (G2[Y, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[Y, Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[Y, Z, ?] |: C)#Point[A]): Pivot[(G2[Y, Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[Y, Z, ?] |: F)#Point, T#Point[A]]]
  }

  implicit def corecurseBarH11[G[_[_], _], H[_], Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G[H, ?] |: C, Pivot, G[H, ?] |: F, T] = new NestedAtPoint[G[H, ?] |: C, Pivot, G[H, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G[H, ?] |: F)#Point, T#Point[A]]): (G[H, ?] |: C)#Point[A] = cc.asInstanceOf[(G[H, ?] |: C)#Point[A]]
    def unpack[A](cc: (G[H, ?] |: C)#Point[A]): Pivot[(G[H, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G[H, ?] |: F)#Point, T#Point[A]]]
  }

  implicit def corecurseBarH12[G[_[_], _], H[_], Pivot[_[_], _, _], R, C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot[?[_], R, ?], F, T]): NestedAtPoint[G[H, ?] |: C, Pivot[?[_], R, ?], G[H, ?] |: F, T] = new NestedAtPoint[G[H, ?] |: C, Pivot[?[_], R, ?], G[H, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G[H, ?] |: F)#Point, R, T#Point[A]]): (G[H, ?] |: C)#Point[A] = cc.asInstanceOf[(G[H, ?] |: C)#Point[A]]
    def unpack[A](cc: (G[H, ?] |: C)#Point[A]): Pivot[(G[H, ?] |: F)#Point, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G[H, ?] |: F)#Point, R, T#Point[A]]]
  }

  implicit def corecurseBarH13[G[_[_], _], H[_], Pivot[_[_], _, _, _], Q, R, C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot[?[_], Q, R, ?], F, T]): NestedAtPoint[G[H, ?] |: C, Pivot[?[_], Q, R, ?], G[H, ?] |: F, T] = new NestedAtPoint[G[H, ?] |: C, Pivot[?[_], Q, R, ?], G[H, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G[H, ?] |: F)#Point, Q, R, T#Point[A]]): (G[H, ?] |: C)#Point[A] = cc.asInstanceOf[(G[H, ?] |: C)#Point[A]]
    def unpack[A](cc: (G[H, ?] |: C)#Point[A]): Pivot[(G[H, ?] |: F)#Point, Q, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G[H, ?] |: F)#Point, Q, R, T#Point[A]]]
  }

  implicit def corecurseBarH21[G[_[_], _, _], G2[_[_], _, _], H[_], Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[H, Z, ?] |: C, Pivot, G2[H, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Z, ?] |: C, Pivot, G2[H, Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[H, Z, ?] |: F)#Point, T#Point[A]]): (G2[H, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[H, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Z, ?] |: F)#Point, T#Point[A]]]
  }

  implicit def corecurseBarH22[G[_[_], _, _], G2[_[_], _, _], H[_], Z, Pivot[_[_], _, _], R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot[?[_], R, ?], F, T]): NestedAtPoint[G2[H, Z, ?] |: C, Pivot[?[_], R, ?], G2[H, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Z, ?] |: C, Pivot[?[_], R, ?], G2[H, Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[H, Z, ?] |: F)#Point, R, T#Point[A]]): (G2[H, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[H, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Z, ?] |: F)#Point, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Z, ?] |: F)#Point, R, T#Point[A]]]
  }

  implicit def corecurseBarH23[G[_[_], _, _], G2[_[_], _, _], H[_], Z, Pivot[_[_], _, _, _], Q, R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot[?[_], Q, R, ?], F, T]): NestedAtPoint[G2[H, Z, ?] |: C, Pivot[?[_], Q, R, ?], G2[H, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Z, ?] |: C, Pivot[?[_], Q, R, ?], G2[H, Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[H, Z, ?] |: F)#Point, Q, R, T#Point[A]]): (G2[H, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[H, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Z, ?] |: F)#Point, Q, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Z, ?] |: F)#Point, Q, R, T#Point[A]]]
  }

  implicit def corecurseBarH31[G[_[_], _, _, _], G2[_[_], _, _, _], H[_], Y, Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot, G2[H, Y, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot, G2[H, Y, Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[H, Y, Z, ?] |: F)#Point, T#Point[A]]): (G2[H, Y, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Y, Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[H, Y, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Y, Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Y, Z, ?] |: F)#Point, T#Point[A]]]
  }

  implicit def corecurseBarH32[G[_[_], _, _, _], G2[_[_], _, _, _], H[_], Y, Z, Pivot[_[_], _, _], R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot[?[_], R, ?], F, T]): NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot[?[_], R, ?], G2[H, Y, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot[?[_], R, ?], G2[H, Y, Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[H, Y, Z, ?] |: F)#Point, R, T#Point[A]]): (G2[H, Y, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Y, Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[H, Y, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Y, Z, ?] |: F)#Point, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Y, Z, ?] |: F)#Point, R, T#Point[A]]]
  }

  implicit def corecurseBarH33[G[_[_], _, _, _], G2[_[_], _, _, _], H[_], Y, Z, Pivot[_[_], _, _, _], Q, R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot[?[_], Q, R, ?], F, T]): NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot[?[_], Q, R, ?], G2[H, Y, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot[?[_], Q, R, ?], G2[H, Y, Z, ?] |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[H, Y, Z, ?] |: F)#Point, Q, R, T#Point[A]]): (G2[H, Y, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Y, Z, ?] |: C)#Point[A]]
    def unpack[A](cc: (G2[H, Y, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Y, Z, ?] |: F)#Point, Q, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Y, Z, ?] |: F)#Point, Q, R, T#Point[A]]]
  }

  implicit def corecursePivot11[G[_[_], _], Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G -|: C, Pivot, G -|: F, T] = new NestedAtPoint[G -|: C, Pivot, G -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G -|: F)#Point, T#Point[A]]): (G -|: C)#Point[A] = cc.asInstanceOf[(G -|: C)#Point[A]]
    def unpack[A](cc: (G -|: C)#Point[A]): Pivot[(G -|: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G -|: F)#Point, T#Point[A]]]
  }

  implicit def corecursePivot12[G[_[_], _], Pivot[_[_], _, _], R, C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot[?[_], R, ?], F, T]): NestedAtPoint[G -|: C, Pivot[?[_], R, ?], G -|: F, T] = new NestedAtPoint[G -|: C, Pivot[?[_], R, ?], G -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G -|: F)#Point, R, T#Point[A]]): (G -|: C)#Point[A] = cc.asInstanceOf[(G -|: C)#Point[A]]
    def unpack[A](cc: (G -|: C)#Point[A]): Pivot[(G -|: F)#Point, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G -|: F)#Point, R, T#Point[A]]]
  }

  implicit def corecursePivot13[G[_[_], _], Pivot[_[_], _, _, _], Q, R, C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot[?[_], Q, R, ?], F, T]): NestedAtPoint[G -|: C, Pivot[?[_], Q, R, ?], G -|: F, T] = new NestedAtPoint[G -|: C, Pivot[?[_], Q, R, ?], G -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G -|: F)#Point, Q, R, T#Point[A]]): (G -|: C)#Point[A] = cc.asInstanceOf[(G -|: C)#Point[A]]
    def unpack[A](cc: (G -|: C)#Point[A]): Pivot[(G -|: F)#Point, Q, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G -|: F)#Point, Q, R, T#Point[A]]]
  }

  implicit def corecursePivot21[G[_[_], _, _], G2[_[_], _, _], Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot, G2[?[_], Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot, G2[?[_], Z, ?] -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[?[_], Z, ?] -|: F)#Point, T#Point[A]]): (G2[?[_], Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (G2[?[_], Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Z, ?] -|: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Z, ?] -|: F)#Point, T#Point[A]]]
  }

  implicit def corecursePivot22[G[_[_], _, _], G2[_[_], _, _], Z, Pivot[_[_], _, _], R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot[?[_], R, ?], F, T]): NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot[?[_], R, ?], G2[?[_], Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot[?[_], R, ?], G2[?[_], Z, ?] -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[?[_], Z, ?] -|: F)#Point, R, T#Point[A]]): (G2[?[_], Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (G2[?[_], Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Z, ?] -|: F)#Point, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Z, ?] -|: F)#Point, R, T#Point[A]]]
  }

  implicit def corecursePivot23[G[_[_], _, _], G2[_[_], _, _], Z, Pivot[_[_], _, _, _], Q, R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot[?[_], Q, R, ?], F, T]): NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot[?[_], Q, R, ?], G2[?[_], Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot[?[_], Q, R, ?], G2[?[_], Z, ?] -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[?[_], Z, ?] -|: F)#Point, Q, R, T#Point[A]]): (G2[?[_], Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (G2[?[_], Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Z, ?] -|: F)#Point, Q, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Z, ?] -|: F)#Point, Q, R, T#Point[A]]]
  }

  implicit def corecursePivot31[G[_[_], _, _, _], G2[_[_], _, _, _], Y, Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot, G2[?[_], Y, Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot, G2[?[_], Y, Z, ?] -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, T#Point[A]]): (G2[?[_], Y, Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Y, Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (G2[?[_], Y, Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, T#Point[A]]]
  }

  implicit def corecursePivot32[G[_[_], _, _, _], G2[_[_], _, _, _], Y, Z, Pivot[_[_], _, _], R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot[?[_], R, ?], F, T]): NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot[?[_], R, ?], G2[?[_], Y, Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot[?[_], R, ?], G2[?[_], Y, Z, ?] -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, R, T#Point[A]]): (G2[?[_], Y, Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Y, Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (G2[?[_], Y, Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, R, T#Point[A]]]
  }

  implicit def corecursePivot33[G[_[_], _, _, _], G2[_[_], _, _, _], Y, Z, Pivot[_[_], _, _, _], Q, R, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot[?[_], Q, R, ?], F, T]): NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot[?[_], Q, R, ?], G2[?[_], Y, Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot[?[_], Q, R, ?], G2[?[_], Y, Z, ?] -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, Q, R, T#Point[A]]): (G2[?[_], Y, Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Y, Z, ?] -|: C)#Point[A]]
    def unpack[A](cc: (G2[?[_], Y, Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, Q, R, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, Q, R, T#Point[A]]]
  }
}