package emm

import scalaz.{Applicative, Bind, Functor, Monad, Traverse}
import scalaz.std.option._
import scalaz.std.list._

import scalaz.syntax.applicative._

sealed trait CoPConst {
  type Point[A]
}

final class |:[F[_], T <: CoPConst] extends CoPConst {
  type Point[A] = F[T#Point[A]]
}

final class CCNil extends CoPConst {
  type Point[A] = A
}

object CoPConst {

  sealed trait CApplicative[C <: CoPConst] {

    def point[A](a: A): C#Point[A]

    def map[A, B](fa: C#Point[A])(f: A => B): C#Point[B]

    def ap[A, B](fa: C#Point[A])(f: C#Point[A => B]): C#Point[B]
  }

  object CApplicative {

    implicit def head[F[_]](implicit F: Applicative[F]): CApplicative[F |: CCNil] = new CApplicative[F |: CCNil] {

      def point[A](a: A) = F.point(a)

      def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)

      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = F.ap(fa)(f)
    }

    implicit def corecurse[F[_], C <: CoPConst](implicit P: CApplicative[C], F: Applicative[F]): CApplicative[F |: C] = new CApplicative[F |: C] {

      def point[A](a: A) = F.point(P.point(a))

      def map[A, B](fa: F[C#Point[A]])(f: A => B): F[C#Point[B]] =
        F.map(fa) { ca => P.map(ca)(f) }

      def ap[A, B](fa: F[C#Point[A]])(f: F[C#Point[A => B]]): F[C#Point[B]] = {
        val f2 = F.map(f) { cf =>
          { ca: C#Point[A] => P.ap(ca)(cf) }
        }

        F.ap(fa)(f2)
      }
    }
  }

  sealed trait CTraverse[C <: CoPConst] {
    def traverse[G[_]: Applicative, A, B](ca: C#Point[A])(f: A => G[B]): G[C#Point[B]]
  }

  object CTraverse {

    implicit def head[F[_]](implicit F: Traverse[F]): CTraverse[F |: CCNil] = new CTraverse[F |: CCNil] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = F.traverse(fa)(f)
    }

    implicit def corecurse[F[_], C <: CoPConst](implicit C: CTraverse[C], F: Traverse[F]): CTraverse[F |: C] = new CTraverse[F |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[C#Point[A]])(f: A => G[B]): G[F[C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }
    }
  }

  sealed trait CJoin[C <: CoPConst] {
    def join[A](cca: C#Point[C#Point[A]]): C#Point[A]
  }

  object CJoin {

    implicit def head[F[_]](implicit F: Bind[F]): CJoin[F |: CCNil] = new CJoin[F |: CCNil] {
      def join[A](ffa: F[F[A]]) = F.join(ffa)
    }

    implicit def corecurse[F[_], C <: CoPConst](implicit C: CJoin[C], T: CTraverse[C], F: Applicative[F], B: Bind[F]): CJoin[F |: C] = new CJoin[F |: C] {

      def join[A](fcfa: F[C#Point[F[C#Point[A]]]]): F[C#Point[A]] = {
        val ffca = F.map(fcfa) { cfa =>
          F.map(T.traverse(cfa) { fa => fa }) { cca =>
            C.join(cca)
          }
        }

        B.join(ffca)
      }
    }
  }

  sealed trait Lifter[F[_], C <: CoPConst] {
    def apply[A](fa: F[A]): C#Point[A]
  }

  object Lifter {

    implicit def exacthead[F[_]]: Lifter[F, F |: CCNil] = new Lifter[F, F |: CCNil] {
      def apply[A](fa: F[A]): F[A] = fa
    }

    implicit def head[F[_], C <: CoPConst](implicit P: CApplicative[C], F: Functor[F]): Lifter[F, F |: C] = new Lifter[F, F |: C] {
      def apply[A](fa: F[A]): F[C#Point[A]] = F.map(fa) { a => P.point(a) }
    }

    implicit def corecurse[F[_], G[_], C <: CoPConst](implicit L: Lifter[F, C], G: Applicative[G]): Lifter[F, G |: C] = new Lifter[F, G |: C] {
      def apply[A](fa: F[A]): G[C#Point[A]] = G.point(L(fa))
    }
  }

  sealed trait Contains[F[_], C <: CoPConst]

  object Contains {

    implicit def head[F[_], C <: CoPConst]: Contains[F, F |: C] =
      new Contains[F, F |: C] {}

    implicit def corecurse[F[_], G[_], C <: CoPConst](implicit C: Contains[F, C]): Contains[F, G |: C] =
      new Contains[F, G |: C] {}
  }
}


final case class Emm[C <: CoPConst, A](run: C#Point[A]) {

  def map[B](f: A => B)(implicit C: CoPConst.CApplicative[C]): Emm[C, B] = Emm(C.map(run)(f))

  def flatMap[B](f: A => Emm[C, B])(implicit A: CoPConst.CApplicative[C], B: CoPConst.CJoin[C]): Emm[C, B] =
    Emm(B.join(A.map(run) { a => f(a).run }))

  def flatMapM[G[_], B](f: A => G[B])(implicit A: CoPConst.CApplicative[C], B: CoPConst.CJoin[C], L: CoPConst.Lifter[G, C]): Emm[C, B] =
    flatMap { a => Emm(L(f(a))) }
}

trait EmmLowPriorityImplicits1 {
  import CoPConst._

  implicit def applicativeInstance[C <: CoPConst](implicit C: CApplicative[C]): Applicative[({ type λ[α] = Emm[C, α] })#λ] = new Applicative[({ type λ[α] = Emm[C, α] })#λ] {

    def point[A](a: => A): Emm[C, A] = new Emm(C.point(a))

    def ap[A, B](fa: => Emm[C, A])(f: => Emm[C, A => B]): Emm[C, B] = new Emm(C.ap(fa.run)(f.run))
  }
}

trait EmmLowPriorityImplicits2 extends EmmLowPriorityImplicits1 {
  import CoPConst._

  implicit def monadInstance[C <: CoPConst : CApplicative : CJoin]: Monad[({ type λ[α] = Emm[C, α] })#λ] = new Monad[({ type λ[α] = Emm[C, α] })#λ] {

    def point[A](a: => A): Emm[C, A] = new Emm(implicitly[CApplicative[C]].point(a))

    def bind[A, B](fa: Emm[C, A])(f: A => Emm[C, B]): Emm[C, B] = fa flatMap f
  }
}

object Emm extends EmmLowPriorityImplicits2 {
  import CoPConst._

  implicit def traverseInstance[C <: CoPConst](implicit C: CTraverse[C]): Traverse[({ type λ[α] = Emm[C, α] })#λ] = new Traverse[({ type λ[α] = Emm[C, α] })#λ] {
    def traverseImpl[G[_]: Applicative, A, B](fa: Emm[C, A])(f: A => G[B]): G[Emm[C, B]] =
      Applicative[G].map(C.traverse(fa.run)(f)) { new Emm(_) }
  }
}