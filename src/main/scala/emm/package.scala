package object emm {
  import Effects._

  implicit class PointSyntax[A](val a: A) extends AnyVal {
    def pointM[C <: Effects](implicit M: Mapper[C]): Emm[C, A] = Emm[C, A](M.point(a))
  }

  implicit class LiftSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def liftM[C <: Effects](implicit L: Lifter[F, C]): Emm[C, A] = new Emm(L(fa))
  }

  implicit class WrapSyntax[E](val e: E) extends AnyVal {
    def wrapM[C <: Effects](implicit W: Wrapper[E, C]): Emm[C, W.A] = Emm[C, W.A](W(e))
  }
}