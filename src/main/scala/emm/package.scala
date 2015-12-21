package object emm {
  import Effects._

  implicit class LiftSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def liftM[C <: Effects](implicit L: Lifter[F, C]): Emm[C, A] = new Emm(L(fa))
  }
}