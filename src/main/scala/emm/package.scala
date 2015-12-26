package object emm {
  import Effects._

  implicit class PointSyntax[A](val a: A) extends AnyVal {
    def pointM[C <: Effects](implicit M: Mapper[C]): Emm[C, A] = Emm[C, A](M.point(a))
  }

  implicit class Lift1Syntax[F[_], A](val fa: F[A]) extends AnyVal {
    def liftM[C <: Effects](implicit L: Lifter[F, C]): Emm[C, A] = new Emm(L(fa))
  }

  implicit class Lift2Syntax[F[_, _], A, B](val fa: F[A, B]) extends AnyVal {
    def liftM[C <: Effects](implicit L: Bilifter[F, A, B, C]): Emm[C, L.Out] = new Emm(L(fa))
  }

  implicit class Lift2HSyntax[F[_[_], _], G[_], A](val fa: F[G, A]) extends AnyVal {
    def liftM[C <: Effects](implicit L: HBilifter[F, G, C]): Emm[C, A] = new Emm(L(fa))
  }

  implicit class WrapSyntax[E](val e: E) extends AnyVal {
    def wrapM[C <: Effects](implicit W: Wrapper[E, C]): Emm[C, W.A] = Emm[C, W.A](W(e))
  }
}