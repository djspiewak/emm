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
  sealed trait Mapper[C <: Effects] {

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

  sealed trait Traverser[C <: Effects] {
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
  sealed trait Binder[C <: Effects] {
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

  sealed trait Expander[C <: Effects] {
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

  sealed trait Collapser[E, C <: Effects] {
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

  sealed trait LeftBilifter[F[_, _], B, C <: Effects] {
    def apply[A](fa: F[A, B]): C#Point[A]
  }

  object LeftBilifter {

    implicit def exacthead[F[_, _], B]: LeftBilifter[F, B, F[?, B] |: Base] = new LeftBilifter[F, B, F[?, B] |: Base] {
      def apply[A](fa: F[A, B]): F[A, B] = fa
    }

    implicit def head[F[_, _], B, C <: Effects](implicit M: Mapper[C], F: Functor[F[?, B]]): LeftBilifter[F, B, F[?, B] |: C] = new LeftBilifter[F, B, F[?, B] |: C] {
      def apply[A](fa: F[A, B]): F[C#Point[A], B] = F.map(fa) { a => M.point(a) }
    }

    implicit def corecurse[F[_, _], G[_], B, C <: Effects](implicit L: LeftBilifter[F, B, C], G: Applicative[G]): LeftBilifter[F, B, G |: C] = new LeftBilifter[F, B, G |: C] {
      def apply[A](fa: F[A, B]): G[C#Point[A]] = G.point(L(fa))
    }

    implicit def pacorecurse[F[_, _], G[_, _], G2[_, _], Z, B, C <: Effects](implicit ev: Permute2[G, G2], L: LeftBilifter[F, B, C], G: Applicative[G2[Z, ?]]): LeftBilifter[F, B, G2[Z, ?] |: C] = new LeftBilifter[F, B, G2[Z, ?] |: C] {
      def apply[A](fa: F[A, B]): G2[Z, C#Point[A]] = G.point(L(fa))
    }

    implicit def corecurseH[F[_, _], G[_[_], _], H[_], B, C <: Effects](implicit L: LeftBilifter[F, B, C], G: Applicative[G[H, ?]]): LeftBilifter[F, B, G[H, ?] |: C] = new LeftBilifter[F, B, G[H, ?] |: C] {
      def apply[A](fa: F[A, B]): G[H, C#Point[A]] = G.point(L(fa))
    }
  }

  sealed trait RightBilifter[F[_, _], B, C <: Effects] {
    def apply[A](fa: F[B, A]): C#Point[A]
  }

  object RightBilifter {

    implicit def exacthead[F[_, _], B]: RightBilifter[F, B, F[B, ?] |: Base] = new RightBilifter[F, B, F[B, ?] |: Base] {
      def apply[A](fa: F[B, A]): F[B, A] = fa
    }

    implicit def head[F[_, _], B, C <: Effects](implicit M: Mapper[C], F: Functor[F[B, ?]]): RightBilifter[F, B, F[B, ?] |: C] = new RightBilifter[F, B, F[B, ?] |: C] {
      def apply[A](fa: F[B, A]): F[B, C#Point[A]] = F.map(fa) { a => M.point(a) }
    }

    implicit def corecurse[F[_, _], G[_], B, C <: Effects](implicit L: RightBilifter[F, B, C], G: Applicative[G]): RightBilifter[F, B, G |: C] = new RightBilifter[F, B, G |: C] {
      def apply[A](fa: F[B, A]): G[C#Point[A]] = G.point(L(fa))
    }

    implicit def pacorecurse[F[_, _], G[_, _], G2[_, _], Z, B, C <: Effects](implicit ev: Permute2[G, G2], L: RightBilifter[F, B, C], G: Applicative[G2[Z, ?]]): RightBilifter[F, B, G2[Z, ?] |: C] = new RightBilifter[F, B, G2[Z, ?] |: C] {
      def apply[A](fa: F[B, A]): G2[Z, C#Point[A]] = G.point(L(fa))
    }

    implicit def corecurseH[F[_, _], G[_[_], _], H[_], B, C <: Effects](implicit L: RightBilifter[F, B, C], G: Applicative[G[H, ?]]): RightBilifter[F, B, G[H, ?] |: C] = new RightBilifter[F, B, G[H, ?] |: C] {
      def apply[A](fa: F[B, A]): G[H, C#Point[A]] = G.point(L(fa))
    }
  }

  @implicitNotFound("could not lift effect ${F} into stack ${C}; either ${C} does not contain ${F}, or there is no Functor for either ${F}[${A}, ?] or ${F}[?, ${B}]")
  sealed trait Bilifter[F[_, _], A, B, C <: Effects] {
    type Out

    def apply(fa: F[A, B]): C#Point[Out]
  }

  object Bilifter {
    type Aux[F[_, _], A, B, C <: Effects, Out0] = Bilifter[F, A, B, C] { type Out = Out0 }

    implicit def left[F[_, _], A, B, C <: Effects](implicit L: LeftBilifter[F, B, C]): Bilifter.Aux[F, A, B, C, A] = new Bilifter[F, A, B, C] {
      type Out = A

      def apply(fa: F[A, B]): C#Point[A] = L(fa)
    }

    implicit def right[F[_, _], A, B, C <: Effects](implicit L: RightBilifter[F, A, C]): Bilifter.Aux[F, A, B, C, B] = new Bilifter[F, A, B, C] {
      type Out = B

      def apply(fa: F[A, B]): C#Point[B] = L(fa)
    }
  }

  @implicitNotFound("could not lift effect ${F}[${G}, ?] into stack ${C}; either ${C} does not contain ${F}, or there is no functor for ${F}[${G}, ?]")
  sealed trait HBilifter[F[_[_], _], G[_], C <: Effects] {
    def apply[A](fa: F[G, A]): C#Point[A]
  }

  object HBilifter {

    implicit def exacthead[F[_[_], _], G[_]]: HBilifter[F, G, F[G, ?] |: Base] = new HBilifter[F, G, F[G, ?] |: Base] {
      def apply[A](fa: F[G, A]): F[G, A] = fa
    }

    implicit def head[F[_[_], _], G[_], C <: Effects](implicit P: Mapper[C], F: Functor[F[G, ?]]): HBilifter[F, G, F[G, ?] |: C] = new HBilifter[F, G, F[G, ?] |: C] {
      def apply[A](fa: F[G, A]) = F.map(fa) { a => P.point(a) }
    }

    implicit def corecurse[F1[_[_], _], G1[_], F2[_], C <: Effects](implicit L: HBilifter[F1, G1, C], F2: Applicative[F2]): HBilifter[F1, G1, F2 |: C] = new HBilifter[F1, G1, F2 |: C] {
      def apply[A](fa: F1[G1, A]): F2[C#Point[A]] = F2.point(L(fa))
    }

    implicit def pacorecurse[F1[_[_], _], G1[_], F2[_, _], F3[_, _], Z, C <: Effects](implicit ev: Permute2[F2, F3], L: HBilifter[F1, G1, C], F3: Applicative[F3[Z, ?]]): HBilifter[F1, G1, F3[Z, ?] |: C] = new HBilifter[F1, G1, F3[Z, ?] |: C] {
      def apply[A](fa: F1[G1, A]): F3[Z, C#Point[A]] = F3.point(L(fa))
    }

    implicit def corecurseH[F1[_[_], _], G1[_], F2[_[_], _], G2[_], C <: Effects](implicit L: HBilifter[F1, G1, C], F2: Applicative[F2[G2, ?]]): HBilifter[F1, G1, F2[G2, ?] |: C] = new HBilifter[F1, G1, F2[G2, ?] |: C] {
      def apply[A](fa: F1[G1, A]): F2[G2, C#Point[A]] = F2.point(L(fa))
    }
  }

  @implicitNotFound("could not lift effect ${F} into stack ${C}; either ${C} does not contain ${F}, or there is no Functor for ${F}")
  sealed trait Lifter[F[_], C <: Effects] {
    def apply[A](fa: F[A]): C#Point[A]
  }

  object Lifter {

    implicit def exacthead[F[_]]: Lifter[F, F |: Base] = new Lifter[F, F |: Base] {
      def apply[A](fa: F[A]): F[A] = fa
    }

    implicit def paexacthead[F[_, _], Z, G[_, _]](implicit ev: Permute2[F, G]): Lifter[G[Z, ?], G[Z, ?] |: Base] = new Lifter[G[Z, ?], G[Z, ?] |: Base] {
      def apply[A](fa: G[Z, A]): G[Z, A] = fa
    }

    implicit def exactheadH[F[_[_], _], G[_]]: Lifter[F[G, ?], F[G, ?] |: Base] = new Lifter[F[G, ?], F[G, ?] |: Base] {
      def apply[A](fa: F[G, A]): F[G, A] = fa
    }

    implicit def head[F[_], C <: Effects](implicit P: Mapper[C], F: Functor[F]): Lifter[F, F |: C] = new Lifter[F, F |: C] {
      def apply[A](fa: F[A]): F[C#Point[A]] = F.map(fa) { a => P.point(a) }
    }

    implicit def pahead[F[_, _], Z, G[_, _], C <: Effects](implicit ev: Permute2[F, G], P: Mapper[C], F: Functor[G[Z, ?]]): Lifter[G[Z, ?], G[Z, ?] |: C] = new Lifter[G[Z, ?], G[Z, ?] |: C] {
      def apply[A](fa: G[Z, A]): G[Z, C#Point[A]] = F.map(fa) { a => P.point(a) }
    }

    implicit def headH[F[_[_], _], G[_], C <: Effects](implicit P: Mapper[C], F: Functor[F[G, ?]]): Lifter[F[G, ?], F[G, ?] |: C] = new Lifter[F[G, ?], F[G, ?] |: C] {
      def apply[A](fa: F[G, A]): F[G, C#Point[A]] = F.map(fa) { a => P.point(a) }
    }

    implicit def corecurse[F[_], G[_], C <: Effects](implicit L: Lifter[F, C], G: Applicative[G]): Lifter[F, G |: C] = new Lifter[F, G |: C] {
      def apply[A](fa: F[A]): G[C#Point[A]] = G.point(L(fa))
    }

    implicit def pacorecurseG[F[_], G[_, _], G2[_, _], Z, C <: Effects](implicit evG: Permute2[G, G2], L: Lifter[F, C], G: Applicative[G2[Z, ?]]): Lifter[F, G2[Z, ?] |: C] = new Lifter[F, G2[Z, ?] |: C] {
      def apply[A](fa: F[A]): G2[Z, C#Point[A]] = G.point(L(fa))
    }

    implicit def pacorecurseF[F[_, _], F2[_, _], Z, G[_], C <: Effects](implicit evF: Permute2[F, F2], L: Lifter[F2[Z, ?], C], G: Applicative[G]): Lifter[F2[Z, ?], G |: C] = new Lifter[F2[Z, ?], G |: C] {
      def apply[A](fa: F2[Z, A]): G[C#Point[A]] = G.point(L(fa))
    }

    implicit def corecurseFG[F[_, _], F2[_, _], ZF, G[_, _], G2[_, _], ZG, C <: Effects](implicit evF: Permute2[F, F2], evG: Permute2[G, G2], L: Lifter[F2[ZF, ?], C], G: Applicative[G2[ZG, ?]]): Lifter[F2[ZF, ?], G2[ZG, ?] |: C] = new Lifter[F2[ZF, ?], G2[ZG, ?] |: C] {
      def apply[A](fa: F2[ZF, A]): G2[ZG, C#Point[A]] = G.point(L(fa))
    }

    implicit def corecurseHF[F[_[_], _], G0[_], G[_], C <: Effects](implicit L: Lifter[F[G0, ?], C], G: Applicative[G]): Lifter[F[G0, ?], G |: C] = new Lifter[F[G0, ?], G |: C] {
      def apply[A](fa: F[G0, A]): G[C#Point[A]] = G.point(L(fa))
    }

    implicit def corecurseHG[F[_], G[_[_], _], H[_], C <: Effects](implicit L: Lifter[F, C], G: Applicative[G[H, ?]]): Lifter[F, G[H, ?] |: C] = new Lifter[F, G[H, ?] |: C] {
      def apply[A](fa: F[A]): G[H, C#Point[A]] = G.point(L(fa))
    }

    implicit def pacorecurseGH[F[_[_], _], G0[_], G[_, _], G2[_, _], Z, C <: Effects](implicit evG: Permute2[G, G2], L: Lifter[F[G0, ?], C], G: Applicative[G2[Z, ?]]): Lifter[F[G0, ?], G2[Z, ?] |: C] = new Lifter[F[G0, ?], G2[Z, ?] |: C] {
      def apply[A](fa: F[G0, A]): G2[Z, C#Point[A]] = G.point(L(fa))
    }

    implicit def pacorecurseFH[F[_, _], F2[_, _], Z, G[_[_], _], H[_], C <: Effects](implicit evF: Permute2[F, F2], L: Lifter[F2[Z, ?], C], G: Applicative[G[H, ?]]): Lifter[F2[Z, ?], G[H, ?] |: C] = new Lifter[F2[Z, ?], G[H, ?] |: C] {
      def apply[A](fa: F2[Z, A]): G[H, C#Point[A]] = G.point(L(fa))
    }
  }

  sealed trait ArbLifter[E, C <: Effects] {
    type Out

    def apply(e: E): C#Point[Out]
  }

  object ArbLifter {
    type Aux[E, C <: Effects, Out0] = ArbLifter[E, C] { type Out = Out0 }

    implicit def lifter[F[_], A, C <: Effects](implicit L: Lifter[F, C]): ArbLifter.Aux[F[A], C, A] = new ArbLifter[F[A], C] {
      type Out = A

      def apply(fa: F[A]) = L(fa)
    }

    implicit def bilifter[F[_, _], A, B, C <: Effects](implicit L: Bilifter[F, A, B, C]): ArbLifter.Aux[F[A, B], C, L.Out] = new ArbLifter[F[A, B], C] {
      type Out = L.Out

      def apply(fa: F[A, B]) = L(fa)
    }

    implicit def hbilifter[F[_[_], _], G[_], A, C <: Effects](implicit L: HBilifter[F, G, C]): ArbLifter.Aux[F[G, A], C, A] = new ArbLifter[F[G, A], C] {
      type Out = A

      def apply(fa: F[G, A]) = L(fa)
    }
  }

  @implicitNotFound("could not infer effect stack ${C} from type ${E}")
  sealed trait Wrapper[E, C <: Effects] {
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

  def flatMapM[E](f: A => E)(implicit E: ArbLifter[E, C], B: Binder[C]): Emm[C, E.Out] =
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