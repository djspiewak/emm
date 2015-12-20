package object emm {
  import CoPConst._

  implicit class LiftSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def liftM[C <: CoPConst](implicit L: Lifter[F, C]): Emm[C, A] = new Emm(L(fa))
  }
}