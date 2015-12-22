package object emm {
  import Effects._

  implicit class LiftSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def liftM[C <: Effects](implicit L: Lifter[F, C]): Emm[C, A] = new Emm(L(fa))
  }

  implicit class WrapSyntax[E](val e: E) extends AnyVal {

    def wrapM[C <: Effects](implicit W: Wrapper[E, C]): Emm[C, W.A] =
      Emm[C, W.A](W(e))
  }
}