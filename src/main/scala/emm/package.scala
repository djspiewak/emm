package object emm {
  import Effects._

  implicit class PointSyntax[A](val a: A) extends AnyVal {
    def pointM[C <: Effects](implicit M: Mapper[C]): Emm[C, A] = Emm[C, A](M.point(a))
  }
/*
  implicit class LiftSyntax[E](val e: E) extends AnyVal {
    def liftM[C <: Effects](implicit L: Lifter[E, C]): Emm[C, L.Out] = Emm(L(e))
  }

  implicit class WrapSyntax[E](val e: E) extends AnyVal {
    def wrapM[C <: Effects](implicit W: Wrapper[E, C]): Emm[C, W.A] = Emm[C, W.A](W(e))
  }*/
}