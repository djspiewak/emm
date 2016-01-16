package emm
package properties

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

  implicit def split2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects](implicit ev: PermuteH2[Pivot, Pivot2], C: NonNested[C]): NestedAtPoint[Pivot2[?[_], Z, ?] -|: C, Pivot2[?[_], Z, ?], Base, C] = split1[Pivot2[?[_], Z, ?], C]
  implicit def split3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects](implicit ev: PermuteH3[Pivot, Pivot2], C: NonNested[C]): NestedAtPoint[Pivot2[?[_], Y, Z, ?] -|: C, Pivot2[?[_], Y, Z, ?], Base, C] = split1[Pivot2[?[_], Y, Z, ?], C]

  implicit def corecurseBar1[Pivot[_[_], _], C <: Effects, C2 <: Effects, F <: Effects, T <: Effects](implicit C2: BarExtract[C, C2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[C2, Pivot, C2.F |: F, T] = new NestedAtPoint[C2, Pivot, C2.F |: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(C2.F |: F)#Point, T#Point[A]]): C2#Point[A] = cc.asInstanceOf[C2#Point[A]]
    def unpack[A](cc: C2#Point[A]): Pivot[(C2.F |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(C2.F |: F)#Point, T#Point[A]]]
  }

  implicit def corecurseBar2[Pivot[_[_], _, _], Z, C <: Effects, C2 <: Effects, F <: Effects, T <: Effects](implicit C2: BarExtract[C, C2], C: NestedAtPoint[C, Pivot[?[_], Z, ?], F, T]): NestedAtPoint[C2, Pivot[?[_], Z, ?], C2.F |: F, T] = corecurseBar1[Pivot[?[_], Z, ?], C, C2, F, T]
  implicit def corecurseBar3[Pivot[_[_], _, _, _], Y, Z, C <: Effects, C2 <: Effects, F <: Effects, T <: Effects](implicit C2: BarExtract[C, C2], C: NestedAtPoint[C, Pivot[?[_], Y, Z, ?], F, T]): NestedAtPoint[C2, Pivot[?[_], Y, Z, ?], C2.F |: F, T] = corecurseBar1[Pivot[?[_], Y, Z, ?], C, C2, F, T]

  // these cases are currently disabled since we have no present use for them and they slow down compilation by roughly 83%
  /*implicit def corecursePivot1[Pivot[_[_], _], C <: Effects, C2 <: Effects, F <: Effects, T <: Effects](implicit E: PivotExtract[C, C2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[C2, Pivot, E.Pivot -|: F, T] = new NestedAtPoint[C2, Pivot, E.Pivot -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(E.Pivot -|: F)#Point, T#Point[A]]): C2#Point[A] = cc.asInstanceOf[C2#Point[A]]
    def unpack[A](cc: C2#Point[A]): Pivot[(E.Pivot -|: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(E.Pivot -|: F)#Point, T#Point[A]]]
  }

  implicit def corecursePivot2[Pivot[_[_], _, _], R, C <: Effects, C2 <: Effects, F <: Effects, T <: Effects](implicit E: PivotExtract[C, C2], C: NestedAtPoint[C, Pivot[?[_], R, ?], F, T]): NestedAtPoint[C2, Pivot[?[_], R, ?], E.Pivot -|: F, T] = new NestedAtPoint[C2, Pivot[?[_], R, ?], E.Pivot -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(E.Pivot -|: F)#Point, R, T#Point[A]]): C2#Point[A] = cc.asInstanceOf[C2#Point[A]]
    def unpack[A](cc: C2#Point[A]): Pivot[(E.Pivot -|: F)#Point, R, T#Point[A]] = cc.asInstanceOf[Pivot[(E.Pivot -|: F)#Point, R, T#Point[A]]]
  }

  implicit def corecursePivot3[Pivot[_[_], _, _, _], Q, R, C <: Effects, C2 <: Effects, F <: Effects, T <: Effects](implicit E: PivotExtract[C, C2], C: NestedAtPoint[C, Pivot[?[_], Q, R, ?], F, T]): NestedAtPoint[C2, Pivot[?[_], Q, R, ?], E.Pivot -|: F, T] = new NestedAtPoint[C2, Pivot[?[_], Q, R, ?], E.Pivot -|: F, T] {
    def NN = C.NN

    def pack[A](cc: Pivot[(E.Pivot -|: F)#Point, Q, R, T#Point[A]]): C2#Point[A] = cc.asInstanceOf[C2#Point[A]]
    def unpack[A](cc: C2#Point[A]): Pivot[(E.Pivot -|: F)#Point, Q, R, T#Point[A]] = cc.asInstanceOf[Pivot[(E.Pivot -|: F)#Point, Q, R, T#Point[A]]]
  }*/
}
