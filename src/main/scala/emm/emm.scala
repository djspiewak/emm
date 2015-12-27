package emm

import scalaz.{Applicative, Bind, Functor, Monad, Traverse}

import scala.annotation.implicitNotFound

sealed trait Effects {
  type Point[A]
}

sealed trait |:[F[_], T <: Effects] extends Effects {
  type Point[A] = F[T#Point[A]]
}

sealed trait Base extends Effects {
  type Point[A] = A
}

object Effects {

  @implicitNotFound("could not compute a method for mapping over effect stack ${C}; either a member of the stack lacks an Applicative, or its Applicative instance is ambiguous")
  trait Mapper[C <: Effects] {

    def point[A](a: A): C#Point[A]

    def map[A, B](fa: C#Point[A])(f: A => B): C#Point[B]

    def ap[A, B](fa: C#Point[A])(f: C#Point[A => B]): C#Point[B]
  }

  object Mapper {

    implicit def head[F[_]](implicit F: Applicative[F]): Mapper[F |: Base] = new Mapper[F |: Base] {

      def point[A](a: A) = F.point(a)

      def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)

      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = F.ap(fa)(f)
    }

    implicit def pahead[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2], F: Applicative[F2[Z, ?]]): Mapper[F2[Z, ?] |: Base] = new Mapper[F2[Z, ?] |: Base] {

      def point[A](a: A) = F.point(a)

      def map[A, B](fa: F2[Z, A])(f: A => B) = F.map(fa)(f)

      def ap[A, B](fa: F2[Z, A])(f: F2[Z, A => B]) = F.ap(fa)(f)
    }

    implicit def headH[F[_[_], _], G[_]](implicit F: Applicative[F[G, ?]]): Mapper[F[G, ?] |: Base] = new Mapper[F[G, ?] |: Base] {

      def point[A](a: A) = F.point(a)

      def map[A, B](fa: F[G, A])(f: A => B): F[G, B] = F.map(fa)(f)

      def ap[A, B](fa: F[G, A])(f: F[G, A => B]): F[G, B] = F.ap(fa)(f)
    }

    implicit def corecurse[F[_], C <: Effects](implicit P: Mapper[C], F: Applicative[F]): Mapper[F |: C] = new Mapper[F |: C] {

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

    implicit def pacorecurse[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], P: Mapper[C], F: Applicative[F2[Z, ?]]): Mapper[F2[Z, ?] |: C] = new Mapper[F2[Z, ?] |: C] {

      def point[A](a: A) = F.point(P.point(a))

      def map[A, B](fa: F2[Z, C#Point[A]])(f: A => B): F2[Z, C#Point[B]] =
        F.map(fa) { ca => P.map(ca)(f) }

      def ap[A, B](fa: F2[Z, C#Point[A]])(f: F2[Z, C#Point[A => B]]): F2[Z, C#Point[B]] = {
        val f2 = F.map(f) { cf =>
          { ca: C#Point[A] => P.ap(ca)(cf) }
        }

        F.ap(fa)(f2)
      }
    }

    implicit def corecurseH[F[_[_], _], G[_], C <: Effects](implicit P: Mapper[C], F: Applicative[F[G, ?]]): Mapper[F[G, ?] |: C] = new Mapper[F[G, ?] |: C] {

      def point[A](a: A) = F.point(P.point(a))

      def map[A, B](fa: F[G, C#Point[A]])(f: A => B): F[G, C#Point[B]] =
        F.map(fa) { ca => P.map(ca)(f) }

      def ap[A, B](fa: F[G, C#Point[A]])(f: F[G, C#Point[A => B]]): F[G, C#Point[B]] = {
        val f2 = F.map(f) { cf =>
          { ca: C#Point[A] => P.ap(ca)(cf) }
        }

        F.ap(fa)(f2)
      }
    }
  }

  trait Traverser[C <: Effects] {
    def traverse[G[_]: Applicative, A, B](ca: C#Point[A])(f: A => G[B]): G[C#Point[B]]
  }

  object Traverser {

    implicit def head[F[_]](implicit F: Traverse[F]): Traverser[F |: Base] = new Traverser[F |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = F.traverse(fa)(f)
    }

    implicit def pahead[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2], F: Traverse[F2[Z, ?]]): Traverser[F2[Z, ?] |: Base] = new Traverser[F2[Z, ?] |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F2[Z, A])(f: A => G[B]): G[F2[Z, B]] = F.traverse(fa)(f)
    }

    implicit def headH[F[_[_], _], G0[_]](implicit F: Traverse[F[G0, ?]]): Traverser[F[G0, ?] |: Base] = new Traverser[F[G0, ?] |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F[G0, A])(f: A => G[B]): G[F[G0, B]] = F.traverse(fa)(f)
    }

    implicit def corecurse[F[_], C <: Effects](implicit C: Traverser[C], F: Traverse[F]): Traverser[F |: C] = new Traverser[F |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[C#Point[A]])(f: A => G[B]): G[F[C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }
    }

    implicit def pacorecurse[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Traverser[C], F: Traverse[F2[Z, ?]]): Traverser[F2[Z, ?] |: C] = new Traverser[F2[Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F2[Z, C#Point[A]])(f: A => G[B]): G[F2[Z, C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }
    }

    implicit def corecurseH[F[_[_], _], G0[_], C <: Effects](implicit C: Traverser[C], F: Traverse[F[G0, ?]]): Traverser[F[G0, ?] |: C] = new Traverser[F[G0, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[G0, C#Point[A]])(f: A => G[B]): G[F[G0, C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }
    }
  }

  @implicitNotFound("could not prove ${C} is a valid monadic stack; perhaps an effect is lacking a Bind, or a non-outer effect is lacking a Traverse")
  trait Binder[C <: Effects] {
    def bind[A, B](cca: C#Point[A])(f: A => C#Point[B]): C#Point[B]
  }

  object Binder {

    implicit def head[F[_]](implicit F: Bind[F]): Binder[F |: Base] = new Binder[F |: Base] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = F.bind(fa)(f)
    }

    implicit def pahead[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2], F: Bind[F2[Z, ?]]): Binder[F2[Z, ?] |: Base] = new Binder[F2[Z, ?] |: Base] {
      def bind[A, B](fa: F2[Z, A])(f: A => F2[Z, B]) = F.bind(fa)(f)
    }

    implicit def headH[F[_[_], _], G[_]](implicit F: Bind[F[G, ?]]): Binder[F[G, ?] |: Base] = new Binder[F[G, ?] |: Base] {
      def bind[A, B](fa: F[G, A])(f: A => F[G, B]): F[G, B] = F.bind(fa)(f)
    }

    implicit def corecurse[F[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], F: Applicative[F], B: Bind[F]): Binder[F |: C] = new Binder[F |: C] {

      def bind[A, B](fca: F[C#Point[A]])(f: A => F[C#Point[B]]): F[C#Point[B]] = {
        B.bind(fca) { ca =>
          val fcaca = T.traverse(ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }

    implicit def pacorecurse[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Binder[C], T: Traverser[C], F: Applicative[F2[Z, ?]], B: Bind[F2[Z, ?]]): Binder[F2[Z, ?] |: C] = new Binder[F2[Z, ?] |: C] {

      def bind[A, B](fca: F2[Z, C#Point[A]])(f: A => F2[Z, C#Point[B]]): F2[Z, C#Point[B]] = {
        B.bind(fca) { ca =>
          val fcaca = T.traverse[F2[Z, ?], A, C#Point[B]](ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }

    implicit def corecurseH[F[_[_], _], G[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], F: Applicative[F[G, ?]], B: Bind[F[G, ?]]): Binder[F[G, ?] |: C] = new Binder[F[G, ?] |: C] {

      def bind[A, B](fca: F[G, C#Point[A]])(f: A => F[G, C#Point[B]]): F[G, C#Point[B]] = {
        B.bind(fca) { ca =>
          val fcaca = T.traverse[F[G, ?], A, C#Point[B]](ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }
  }

  trait Expander[C <: Effects] {
    type CC[_]
    type Out <: Effects

    def apply[A](fa: C#Point[A]): Out#Point[CC[A]]
  }

  object Expander {
    type Aux[C <: Effects, CC0[_], Out0 <: Effects] = Expander[C] { type CC[A] = CC0[A]; type Out = Out0 }

    implicit def head[F[_]]: Expander.Aux[F |: Base, F, Base] = new Expander[F |: Base] {
      type CC[A] = F[A]
      type Out = Base

      def apply[A](fa: F[A]): F[A] = fa
    }

    implicit def pahead[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2]): Expander.Aux[F2[Z, ?] |: Base, F2[Z, ?], Base] = new Expander[F2[Z, ?] |: Base] {
      type CC[A] = F2[Z, A]
      type Out = Base

      def apply[A](fa: F2[Z, A]): F2[Z, A] = fa
    }

    implicit def headH[F[_[_], _], G[_]]: Expander.Aux[F[G, ?] |: Base, F[G, ?], Base] = new Expander[F[G, ?] |: Base] {
      type CC[A] = F[G, A]
      type Out = Base

      def apply[A](fa: F[G, A]): F[G, A] = fa
    }

    implicit def corecurse[F[_], C <: Effects](implicit C: Expander[C]): Expander.Aux[F |: C, C.CC, F |: C.Out] = new Expander[F |: C] {
      type CC[A] = C.CC[A]
      type Out = F |: C.Out

      def apply[A](gca: F[C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def pacorecurse[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Expander[C]): Expander.Aux[F2[Z, ?] |: C, C.CC, F2[Z, ?] |: C.Out] = new Expander[F2[Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[Z, ?] |: C.Out

      def apply[A](gca: F2[Z, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH[F[_[_], _], G[_], C <: Effects](implicit C: Expander[C]): Expander.Aux[F[G, ?] |: C, C.CC, F[G, ?] |: C.Out] = new Expander[F[G, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F[G, ?] |: C.Out

      def apply[A](gca: F[G, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }
  }

  trait Collapser[E, C <: Effects] {
    type A
    type Out <: Effects

    def apply(fa: C#Point[E]): Out#Point[A]
  }

  object Collapser {
    type Aux[E, C <: Effects, A0, Out0 <: Effects] = Collapser[E, C] { type A = A0; type Out = Out0 }

    implicit def head[F[_], A0]: Collapser.Aux[F[A0], Base, A0, F |: Base] = new Collapser[F[A0], Base] {
      type A = A0
      type Out = F |: Base

      def apply(fa: F[A]): F[A] = fa
    }

    implicit def pahead[F[_, _], F2[_, _], Z, A0](implicit ev: Permute2[F, F2]): Collapser.Aux[F2[Z, A0], Base, A0, F2[Z, ?] |: Base] = new Collapser[F2[Z, A0], Base] {
      type A = A0
      type Out = F2[Z, ?] |: Base

      def apply(fa: F2[Z, A]): F2[Z, A] = fa
    }

    implicit def headH[F[_[_], _], G[_], A0]: Collapser.Aux[F[G, A0], Base, A0, F[G, ?] |: Base] = new Collapser[F[G, A0], Base] {
      type A = A0
      type Out = F[G, ?] |: Base

      def apply(fa: F[G, A]): F[G, A] = fa
    }

    implicit def corecurse[E, F[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F |: C, C.A, F |: C.Out] = new Collapser[E, F |: C] {
      type A = C.A
      type Out = F |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F[C#Point[E]]): F[C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def pacorecurse[E, F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[Z, ?] |: C, C.A, F2[Z, ?] |: C.Out] = new Collapser[E, F2[Z, ?] |: C] {
      type A = C.A
      type Out = F2[Z, ?] |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F2[Z, C#Point[E]]): F2[Z, C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH[E, F[_[_], _], G[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F[G, ?] |: C, C.A, F[G, ?] |: C.Out] = new Collapser[E, F[G, ?] |: C] {
      type A = C.A
      type Out = F[G, ?] |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F[G, C#Point[E]]): F[G, C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }
  }

  @implicitNotFound("could not lift ${E} into stack ${C}; either ${C} does not contain a constructor of ${E}, or there is no Functor for a constructor of ${E}")
  trait Lifter[E, C <: Effects] {
    type Out

    def apply(e: E): C#Point[Out]
  }

  object Lifter {
    type Aux[E, C <: Effects, Out0] = Lifter[E, C] { type Out = Out0 }

    implicit def head1[F[_], A]: Lifter.Aux[F[A], F |: Base, A] = new Lifter[F[A], F |: Base] {
      type Out = A

      def apply(fa: F[A]) = fa
    }

    implicit def head2[F[_, _], F2[_, _], Z, A](implicit ev: Permute2[F, F2]): Lifter.Aux[F2[Z, A], F2[Z, ?] |: Base, A] = new Lifter[F2[Z, A], F2[Z, ?] |: Base] {
      type Out = A

      def apply(fa: F2[Z, A]) = fa
    }

    implicit def headH1[F[_[_], _], G[_], A]: Lifter.Aux[F[G, A], F[G, ?] |: Base, A] = new Lifter[F[G, A], F[G, ?] |: Base] {
      type Out = A

      def apply(fa: F[G, A]) = fa
    }

    implicit def mid1[F[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F]): Lifter.Aux[F[A], F |: C, A] = new Lifter[F[A], F |: C] {
      type Out = A

      def apply(fa: F[A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def mid2[F[_, _], F2[_, _], Z, A, C <: Effects](implicit ev: Permute2[F, F2], C: Mapper[C], F: Functor[F2[Z, ?]]): Lifter.Aux[F2[Z, A], F2[Z, ?] |: C, A] = new Lifter[F2[Z, A], F2[Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[Z, A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def midH1[F[_[_], _], G[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F[G, ?]]): Lifter.Aux[F[G, A], F[G, ?] |: C, A] = new Lifter[F[G, A], F[G, ?] |: C] {
      type Out = A

      def apply(fa: F[G, A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def corecurse1[F[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F]): Lifter.Aux[E, F |: C, L.Out] = new Lifter[E, F |: C] {
      type Out = L.Out

      def apply(e: E) = F.point(L(e))
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects](implicit ev: Permute2[F, F2], L: Lifter[E, C], F: Applicative[F2[Z, ?]]): Lifter.Aux[E, F2[Z, ?] |: C, L.Out] = new Lifter[E, F2[Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = F.point(L(e))
    }

    implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F[G, ?]]): Lifter.Aux[E, F[G, ?] |: C, L.Out] = new Lifter[E, F[G, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = F.point(L(e))
    }
  }

  @implicitNotFound("could not infer effect stack ${C} from type ${E}")
  trait Wrapper[E, C <: Effects] {
    type A

    def apply(e: E): C#Point[A]
  }

  trait WrapperLowPriorityImplicits {

    implicit def head[A0]: Wrapper.Aux[A0, Base, A0] = new Wrapper[A0, Base] {
      type A = A0

      def apply(a: A) = a
    }
  }

  object Wrapper extends WrapperLowPriorityImplicits {
    type Aux[E, C <: Effects, A0] = Wrapper[E, C] { type A = A0 }

    implicit def corecurse[F[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[E], F |: C, A0] = new Wrapper[F[E], F |: C] {
      type A = A0

      def apply(fe: F[E]): F[C#Point[A]] =
        fe.asInstanceOf[F[C#Point[A]]]      // already proven equivalent; actual evaluation requires a Functor
    }

    implicit def pacorecurse[F[_, _], F2[_, _], Z, E, C <: Effects, A0](implicit ev: Permute2[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[Z, E], F2[Z, ?] |: C, A0] = new Wrapper[F2[Z, E], F2[Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[Z, E]): F2[Z, C#Point[A]] =
        fe.asInstanceOf[F2[Z, C#Point[A]]]
    }

    implicit def corecurseH[F[_[_], _], G[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[G, E], F[G, ?] |: C, A0] = new Wrapper[F[G, E], F[G, ?] |: C] {
      type A = A0

      def apply(fe: F[G, E]): F[G, C#Point[A]] =
        fe.asInstanceOf[F[G, C#Point[A]]]      // already proven equivalent; actual evaluation requires a Functor
    }
  }
}


final case class Emm[C <: Effects, A](run: C#Point[A]) {
  import Effects._

  def map[B](f: A => B)(implicit C: Mapper[C]): Emm[C, B] = Emm(C.map(run)(f))

  def flatMap[B](f: A => Emm[C, B])(implicit B: Binder[C]): Emm[C, B] =
    Emm(B.bind(run) { a => f(a).run })

  def flatMapM[E](f: A => E)(implicit E: Lifter[E, C], B: Binder[C]): Emm[C, E.Out] =
    flatMap { a => Emm(E(f(a))) }

  def expand(implicit C: Expander[C]): Emm[C.Out, C.CC[A]] = Emm(C(run))

  def collapse(implicit C: Collapser[A, C]): Emm[C.Out, C.A] = Emm(C(run))
}

trait EmmLowPriorityImplicits1 {
  import Effects._

  implicit def applicativeInstance[C <: Effects](implicit C: Mapper[C]): Applicative[Emm[C, ?]] = new Applicative[Emm[C, ?]] {

    def point[A](a: => A): Emm[C, A] = new Emm(C.point(a))

    def ap[A, B](fa: => Emm[C, A])(f: => Emm[C, A => B]): Emm[C, B] = new Emm(C.ap(fa.run)(f.run))
  }
}

trait EmmLowPriorityImplicits2 extends EmmLowPriorityImplicits1 {
  import Effects._

  implicit def monadInstance[C <: Effects : Mapper : Binder]: Monad[Emm[C, ?]] = new Monad[Emm[C, ?]] {

    def point[A](a: => A): Emm[C, A] = new Emm(implicitly[Mapper[C]].point(a))

    def bind[A, B](fa: Emm[C, A])(f: A => Emm[C, B]): Emm[C, B] = fa flatMap f
  }
}

object Emm extends EmmLowPriorityImplicits2 {
  import Effects._

  implicit def traverseInstance[C <: Effects](implicit C: Traverser[C]): Traverse[Emm[C, ?]] = new Traverse[Emm[C, ?]] {
    def traverseImpl[G[_]: Applicative, A, B](fa: Emm[C, A])(f: A => G[B]): G[Emm[C, B]] =
      Applicative[G].map(C.traverse(fa.run)(f)) { new Emm(_) }
  }
}