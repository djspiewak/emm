package object emm {
  import effects._

  implicit class Syntax[A](val a: A) extends AnyVal {
    def pointM[C <: Effects](implicit M: Mapper[C]): Emm[C, A] = Emm.point[C, A](a)
    def liftM[C <: Effects](implicit L: Lifter[A, C]): Emm[C, L.Out] = Emm.lift[C, A](a)
    def wrapM[C <: Effects](implicit W: Wrapper[A, C]): Emm[C, W.A] = Emm.wrap[C, A](a)
  }
}
