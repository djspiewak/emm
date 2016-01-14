package emm

final case class Emm[C <: Effects, A](run: C#Point[A]) {
  import effects._

  def map[B](f: A => B)(implicit C: Mapper[C]): Emm[C, B] = Emm(C.map(run)(f))

  def flatMap[B](f: A => Emm[C, B])(implicit B: Binder[C]): Emm[C, B] =
    Emm(B.bind(run) { a => f(a).run })

  def flatMapM[E](f: A => E)(implicit E: Lifter[E, C], B: Binder[C]): Emm[C, E.Out] =
    flatMap { a => Emm(E(f(a))) }

  def expand(implicit C: Expander[C]): Emm[C.Out, C.CC[A]] = Emm(C(run))

  def collapse(implicit C: Collapser[A, C]): Emm[C.Out, C.A] = Emm(C(run))
}

/*trait EmmLowPriorityImplicits1 {
  import effects._

  implicit def functorInstance[C <: Effects](implicit C: Mapper[C]): Functor[Emm[C, ?]] = new Functor[Emm[C, ?]] {

    def map[A, B](fa: Emm[C, A])(f: A => B): Emm[C, B] = new Emm(C.map(fa.run)(f))
  }
}

trait EmmLowPriorityImplicits2 extends EmmLowPriorityImplicits1 {
  import effects._

  implicit def monadInstance[C <: Effects : Mapper : Binder]: Monad[Emm[C, ?]] = new Monad[Emm[C, ?]] {

    def pure[A](a: A): Emm[C, A] = new Emm(implicitly[Mapper[C]].point(a))

    def flatMap[A, B](fa: Emm[C, A])(f: A => Emm[C, B]): Emm[C, B] = fa flatMap f
  }
}

object Emm extends EmmLowPriorityImplicits2 {
  import effects._

  implicit def traverseInstance[C <: Effects](implicit C: Traverser[C]): Traverse[Emm[C, ?]] = new Traverse[Emm[C, ?]] {
    def traverse[G[_]: Applicative, A, B](fa: Emm[C, A])(f: A => G[B]): G[Emm[C, B]] =
      Applicative[G].map(C.traverse(fa.run)(f)) { new Emm(_) }

    def foldLeft[A, B](fa: Emm[C, A], b: B)(f: (B, A) => B): B = C.foldLeft(fa.run, b)(f)

    def foldRight[A, B](fa: Emm[C, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = C.foldRight(fa.run, lb)(f)

  }
}*/
