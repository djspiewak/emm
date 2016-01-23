package emm
package compat

import effects._
import properties._

import _root_.cats.~>
import _root_.cats.data.Kleisli

private[emm] object Shared {

  implicit def functor[F <: Effects](implicit F: Mapper[F]): _root_.cats.Functor[F#Point] = new _root_.cats.Functor[F#Point] {
    def map[A, B](fa: F#Point[A])(f: A => B): F#Point[B] = F.map(fa)(f)
  }

  implicit def monad[F <: Effects](implicit FM: Mapper[F], FB: Binder[F]): _root_.cats.Monad[F#Point] = new _root_.cats.Monad[F#Point] {
    def pure[A](a: A) = FM.point(a)
    override def map[A, B](fa: F#Point[A])(f: A => B): F#Point[B] = FM.map(fa)(f)
    def flatMap[A, B](fa: F#Point[A])(f: A => F#Point[B]): F#Point[B] = FB.bind(fa)(f)
  }
}

private[emm] trait BinderShims {

  implicit def pivotKleisliBinder[Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], FB: Binder[F], FM: Mapper[F], TB: Binder[T], TT: Traverser[T]): Binder[C] = new Binder[C] {
    import Shared.monad

    def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
      val back = NAP.unpack(fca) flatMap { ca =>
        val ptta = TT.traverse[Kleisli[F#Point, Z, ?], A, T#Point[B]](ca)({ a => NAP.unpack(f(a)) })(shims.cats.applicative1[Kleisli[F#Point, Z, ?]](Kleisli.kleisliApplicative[F#Point, Z]))

        ptta map { tta => TB.bind(tta) { a => a } }
      }

      NAP.pack(back)
    }
  }
}

private[emm] trait LifterShims {

  implicit def midKleisli[A, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], F: Mapper[F], T: Mapper[T]): Lifter.Aux[Kleisli[λ[X => X], Z, A], C, A] = new Lifter[Kleisli[λ[X => X], Z, A], C] {
    type Out = A

    import Shared.functor

    def apply(e: Kleisli[λ[X => X], Z, A]): CC[Out] = {
      val t = new (λ[X => X] ~> F#Point) {
        def apply[A](a: A): F#Point[A] = F.point(a)
      }

      NAP.pack(e.transform[F#Point](t) map T.point)
    }
  }

  implicit def leftPivotKleisli[E, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], T: Mapper[T], F: Lifter[E, F], FM: Mapper[F]): Lifter.Aux[E, C, F.Out] = new Lifter[E, C] {
    type Out = F.Out

    def apply(e: E): CC[Out] =
      NAP.pack(Kleisli[F#Point, Z, T#Point[F.Out]]({ _ => FM.map(F(e)) { a => T.point(a) } }))
  }

  implicit def rightPivotKleisli[E, Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], T: Lifter[E, T], F: Mapper[F]): Lifter.Aux[E, C, T.Out] = new Lifter[E, C] {
    type Out = T.Out

    def apply(e: E): CC[Out] = NAP.pack(Kleisli[F#Point, Z, T#Point[T.Out]] { _ => F.point(T(e)) })
  }
}

private[emm] trait MapperShims {

  // we require a Binder[F] because we cannot derive a consistent Applicative from Mapper[F]
  implicit def pivotKleisliMapper[Z, C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Kleisli[?[_], Z, ?], F, T], FB: Binder[F], FM: Mapper[F], T: Mapper[T]): Mapper[C] = new Mapper[C] {
    import Shared.monad

    def point[A](a: A): CC[A] = NAP.pack(Kleisli.pure[F#Point, Z, T#Point[A]](T.point(a)))

    def map[A, B](fa: CC[A])(f: A => B): CC[B] =
      NAP.pack(NAP.unpack(fa) map { ta => T.map(ta)(f) })
  }
}

object cats extends shims.Implicits with BinderShims with LifterShims with MapperShims