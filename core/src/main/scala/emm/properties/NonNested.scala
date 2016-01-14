package emm
package properties

/**
 * The property of Effects which do not contain a -|: case
 */
sealed trait NonNested[C <: Effects] {
  def pack[CC[_], A](cc: CC[C#Point[A]]): C#Build[CC, A]
  def unpack[CC[_], A](cc: C#Build[CC, A]): CC[C#Point[A]]
}

object NonNested {

  implicit def base: NonNested[Base] = new NonNested[Base] {
    def pack[CC[_], A](cc: CC[A]) = cc
    def unpack[CC[_], A](cc: CC[A]) = cc
  }

  implicit def corecurse1[F[_], C <: Effects](implicit C: NonNested[C]): NonNested[F |: C] = new NonNested[F |: C] {
    def pack[CC[_], A](cc: CC[(F |: C)#Point[A]]) = cc.asInstanceOf[(F |: C)#Build[CC, A]]
    def unpack[CC[_], A](cc: (F |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F |: C)#Point[A]]]
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: NonNested[C]): NonNested[F2[Z, ?] |: C] = new NonNested[F2[Z, ?] |: C] {
    def pack[CC[_], A](cc: CC[(F2[Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[Z, ?] |: C)#Build[CC, A]]
    def unpack[CC[_], A](cc: (F2[Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[Z, ?] |: C)#Point[A]]]
  }

  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: NonNested[C]): NonNested[F2[Y, Z, ?] |: C] = new NonNested[F2[Y, Z, ?] |: C] {
    def pack[CC[_], A](cc: CC[(F2[Y, Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[Y, Z, ?] |: C)#Build[CC, A]]
    def unpack[CC[_], A](cc: (F2[Y, Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[Y, Z, ?] |: C)#Point[A]]]
  }

  implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: NonNested[C]): NonNested[F[G, ?] |: C] = new NonNested[F[G, ?] |: C] {
    def pack[CC[_], A](cc: CC[(F[G, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F[G, ?] |: C)#Build[CC, A]]
    def unpack[CC[_], A](cc: (F[G, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F[G, ?] |: C)#Point[A]]]
  }

  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: NonNested[C]): NonNested[F2[G, Z, ?] |: C] = new NonNested[F2[G, Z, ?] |: C] {
    def pack[CC[_], A](cc: CC[(F2[G, Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[G, Z, ?] |: C)#Build[CC, A]]
    def unpack[CC[_], A](cc: (F2[G, Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[G, Z, ?] |: C)#Point[A]]]
  }

  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: NonNested[C]): NonNested[F2[G, Y, Z, ?] |: C] = new NonNested[F2[G, Y, Z, ?] |: C] {
    def pack[CC[_], A](cc: CC[(F2[G, Y, Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[G, Y, Z, ?] |: C)#Build[CC, A]]
    def unpack[CC[_], A](cc: (F2[G, Y, Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[G, Y, Z, ?] |: C)#Point[A]]]
  }
}