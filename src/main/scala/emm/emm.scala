package emm

import scalaz.{Applicative, Bind, Functor, Monad, Traverse}

import scala.annotation.implicitNotFound

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

  @implicitNotFound("could not compute a method for mapping over effect stack ${C}; either a member of the stack lacks an Applicative, or its Applicative instance is ambiguous")
  sealed trait Mapper[C <: CoPConst] {

    def point[A](a: A): C#Point[A]

    def map[A, B](fa: C#Point[A])(f: A => B): C#Point[B]

    def ap[A, B](fa: C#Point[A])(f: C#Point[A => B]): C#Point[B]
  }

  object Mapper {

    implicit def head[F[_]](implicit F: Applicative[F]): Mapper[F |: CCNil] = new Mapper[F |: CCNil] {

      def point[A](a: A) = F.point(a)

      def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)

      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = F.ap(fa)(f)
    }

    implicit def corecurse[F[_], C <: CoPConst](implicit P: Mapper[C], F: Applicative[F]): Mapper[F |: C] = new Mapper[F |: C] {

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

  sealed trait Traverser[C <: CoPConst] {
    def traverse[G[_]: Applicative, A, B](ca: C#Point[A])(f: A => G[B]): G[C#Point[B]]
  }

  object Traverser {

    implicit def head[F[_]](implicit F: Traverse[F]): Traverser[F |: CCNil] = new Traverser[F |: CCNil] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = F.traverse(fa)(f)
    }

    implicit def corecurse[F[_], C <: CoPConst](implicit C: Traverser[C], F: Traverse[F]): Traverser[F |: C] = new Traverser[F |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[C#Point[A]])(f: A => G[B]): G[F[C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }
    }
  }

  @implicitNotFound("could not prove ${C} is a valid monadic stack; perhaps an effect is lacking a Bind, or a non-outer effect is lacking a Traverse")
  sealed trait Joiner[C <: CoPConst] {
    def join[A](cca: C#Point[C#Point[A]]): C#Point[A]
  }

  object Joiner {

    implicit def head[F[_]](implicit F: Bind[F]): Joiner[F |: CCNil] = new Joiner[F |: CCNil] {
      def join[A](ffa: F[F[A]]) = F.join(ffa)
    }

    implicit def corecurse[F[_], C <: CoPConst](implicit C: Joiner[C], T: Traverser[C], F: Applicative[F], B: Bind[F]): Joiner[F |: C] = new Joiner[F |: C] {

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

  sealed trait Expander[F[_], C <: CoPConst, Out <: CoPConst] {

    def apply[A](fa: C#Point[A]): Out#Point[F[A]]
  }

  object Expander {

    implicit def head[F[_]]: Expander[F, F |: CCNil, CCNil] = new Expander[F, F |: CCNil, CCNil] {
      def apply[A](fa: F[A]): F[A] = fa
    }

    implicit def corecurse[F[_], G[_], C <: CoPConst, C2 <: CoPConst](implicit C: Expander[F, C, C2], G: Functor[G]): Expander[F, G |: C, G |: C2] = new Expander[F, G |: C, G |: C2] {

      def apply[A](gca: G[C#Point[A]]): G[C2#Point[F[A]]] =
        G.map(gca) { ca => C(ca) }
    }
  }

  sealed trait Collapser[F[_], C <: CoPConst, Out <: CoPConst] {

    def apply[A](fa: C#Point[F[A]]): Out#Point[A]
  }

  object Collapser {

    implicit def head[F[_]]: Collapser[F, CCNil, F |: CCNil] = new Collapser[F, CCNil, F |: CCNil] {

      def apply[A](fa: F[A]): F[A] = fa
    }

    implicit def corecurse[F[_], G[_], C <: CoPConst, C2 <: CoPConst](implicit C: Collapser[F, C, C2], G: Functor[G]): Collapser[F, G |: C, G |: C2] = new Collapser[F, G |: C, G |: C2] {

      def apply[A](gca: G[C#Point[F[A]]]): G[C2#Point[A]] =
        G.map(gca) { ca => C(ca) }
    }
  }

  @implicitNotFound("could not lift effect ${F} into stack ${C}; either ${C} does not contain ${F}, or there is no Functor for ${F}")
  sealed trait Lifter[F[_], C <: CoPConst] {
    def apply[A](fa: F[A]): C#Point[A]
  }

  object Lifter {

    implicit def exacthead[F[_]]: Lifter[F, F |: CCNil] = new Lifter[F, F |: CCNil] {
      def apply[A](fa: F[A]): F[A] = fa
    }

    implicit def head[F[_], C <: CoPConst](implicit P: Mapper[C], F: Functor[F]): Lifter[F, F |: C] = new Lifter[F, F |: C] {
      def apply[A](fa: F[A]): F[C#Point[A]] = F.map(fa) { a => P.point(a) }
    }

    implicit def corecurse[F[_], G[_], C <: CoPConst](implicit L: Lifter[F, C], G: Applicative[G]): Lifter[F, G |: C] = new Lifter[F, G |: C] {
      def apply[A](fa: F[A]): G[C#Point[A]] = G.point(L(fa))
    }
  }

  @implicitNotFound("could not lift ${E} into effect stack ${C}; either ${C} does not contain a component of ${E}, or a component of ${E} is missing a Functor")
  sealed trait UnappliedLifter[E, C <: CoPConst] {
    type Out

    def apply(e: E): C#Point[Out]
  }

  trait UnappliedLifterLowPriorityImplicits {

    implicit def identity[Out0, C <: CoPConst]: UnappliedLifter.Aux[Emm[C, Out0], C, Out0] = new UnappliedLifter[Emm[C, Out0], C] {
      type Out = Out0

      def apply(emm: Emm[C, Out]) = emm.run
    }
  }

  object UnappliedLifter extends UnappliedLifterLowPriorityImplicits {
    type Aux[E, C <: CoPConst, Out0] = UnappliedLifter[E, C] { type Out = Out0 }

    implicit def exacthead[F[_], Out0]: UnappliedLifter.Aux[F[Out0], F |: CCNil, Out0] = new UnappliedLifter[F[Out0], F |: CCNil] {
      type Out = Out0

      def apply(fa: F[Out]) = fa
    }

    implicit def head[F[_], Out0, C <: CoPConst](implicit P: Mapper[C], F: Functor[F]): UnappliedLifter.Aux[F[Out0], F |: C, Out0] = new UnappliedLifter[F[Out0], F |: C]{
      type Out = Out0

      def apply(fa: F[Out]): F[C#Point[Out]] = F.map(fa) { a => P.point(a) }
    }

    implicit def corecurse[F[_], Out0, G[_], C <: CoPConst](implicit M: UnappliedLifter.Aux[F[Out0], C, Out0], G: Applicative[G]): UnappliedLifter.Aux[F[Out0], G |: C, Out0] = new UnappliedLifter[F[Out0], G |: C] {
      type Out = Out0

      def apply(fa: F[Out]): G[C#Point[Out]] = G.point(M(fa))
    }
  }
}


final case class Emm[C <: CoPConst, A](run: C#Point[A]) {

  def map[B](f: A => B)(implicit C: CoPConst.Mapper[C]): Emm[C, B] = Emm(C.map(run)(f))

  def flatMap[B](f: A => Emm[C, B])(implicit A: CoPConst.Mapper[C], B: CoPConst.Joiner[C]): Emm[C, B] =
    Emm(B.join(A.map(run) { a => f(a).run }))

  def flatMapM[E, B](f: A => E)(implicit L: CoPConst.UnappliedLifter[E, C], A: CoPConst.Mapper[C], B: CoPConst.Joiner[C]): Emm[C, L.Out] =
    flatMap { a => Emm(L(f(a))) }

  def expand[G[_], C2 <: CoPConst](implicit C: CoPConst.Expander[G, C, C2]): Emm[C2, G[A]] =
    Emm(C(run))

  def collapse[G[_], B, C2 <: CoPConst](implicit ev: A =:= G[B], C: CoPConst.Collapser[G, C, C2]): Emm[C2, B] =
    Emm(C(run.asInstanceOf[C#Point[G[B]]]))     // cast is just to avoid unnecessary mapping
}

trait EmmLowPriorityImplicits1 {
  import CoPConst._

  implicit def applicativeInstance[C <: CoPConst](implicit C: Mapper[C]): Applicative[({ type λ[α] = Emm[C, α] })#λ] = new Applicative[({ type λ[α] = Emm[C, α] })#λ] {

    def point[A](a: => A): Emm[C, A] = new Emm(C.point(a))

    def ap[A, B](fa: => Emm[C, A])(f: => Emm[C, A => B]): Emm[C, B] = new Emm(C.ap(fa.run)(f.run))
  }
}

trait EmmLowPriorityImplicits2 extends EmmLowPriorityImplicits1 {
  import CoPConst._

  implicit def monadInstance[C <: CoPConst : Mapper : Joiner]: Monad[({ type λ[α] = Emm[C, α] })#λ] = new Monad[({ type λ[α] = Emm[C, α] })#λ] {

    def point[A](a: => A): Emm[C, A] = new Emm(implicitly[Mapper[C]].point(a))

    def bind[A, B](fa: Emm[C, A])(f: A => Emm[C, B]): Emm[C, B] = fa flatMap f
  }
}

object Emm extends EmmLowPriorityImplicits2 {
  import CoPConst._

  implicit def traverseInstance[C <: CoPConst](implicit C: Traverser[C]): Traverse[({ type λ[α] = Emm[C, α] })#λ] = new Traverse[({ type λ[α] = Emm[C, α] })#λ] {
    def traverseImpl[G[_]: Applicative, A, B](fa: Emm[C, A])(f: A => G[B]): G[Emm[C, B]] =
      Applicative[G].map(C.traverse(fa.run)(f)) { new Emm(_) }
  }
}