package emm

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}

import scala.annotation.implicitNotFound

sealed trait Effects {
  type Point[A] = Build[λ[X => X], A]

  type Build[CC[_], A]
}

sealed trait |:[F[_], T <: Effects] extends Effects {
  type Build[CC[_], A] = T#Build[λ[X => CC[F[X]]], A]
}

sealed trait -|:[F[_[_], _], T <: Effects] extends Effects {
  type Build[CC[_], A] = T#Build[λ[X => F[CC, X]], A]
}

sealed trait Base extends Effects {
  type Build[CC[_], A] = CC[A]
}

object Properties {

  /**
   * The property of Effects which do not contain a -|: case
   */
  sealed trait NonNested[C <: Effects] {
    def pack[CC[_], A](cc: CC[C#Point[A]]): C#Build[CC, A]
    def unpack[CC[_], A](cc: C#Build[CC, A]): CC[C#Point[A]]
  }

  object NonNested {

    // TODO other cases
    implicit def head[F[_]]: NonNested[F |: Base] = new NonNested[F |: Base] {
      def pack[CC[_], A](cc: CC[F[A]]) = cc
      def unpack[CC[_], A](cc: CC[F[A]]) = cc
    }

    implicit def corecurse[F[_], C <: Effects](implicit C: NonNested[C]): NonNested[F |: C] = new NonNested[F |: C] {
      def pack[CC[_], A](cc: CC[(F |: C)#Point[A]]) = cc.asInstanceOf[(F |: C)#Build[CC, A]]
      def unpack[CC[_], A](cc: (F |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F |: C)#Point[A]]]
    }
  }

  /**
   * The property of Effects which contain at least one -|: case, partitioning into a front and tail, where the front
   * is NonNested and the tail is unconstrained.
   */
  sealed trait NestedAtPoint[C <: Effects] {
    type Nest[F[_], A]
    type Front <: Effects
    type Tail <: Effects

    def apply[A](cc: Nest[Front#Point, Tail#Point[A]]): C#Point[A]
  }

  object NestedAtPoint {
    type Aux[C <: Effects, Nest0[_[_], _], Front0 <: Effects, Tail0 <: Effects] = NestedAtPoint[C] { type Nest[F[_], A] = Nest0[F, A]; type Front = Front0; type Tail = Tail0 }

    implicit def split[Nest0[_[_], _], C <: Effects]: NestedAtPoint.Aux[Nest0 -|: C, Nest0, Base, C] = new NestedAtPoint[Nest0 -|: C] {
      type Nest[F[_], A] = Nest0[F, A]
      type Front = Base
      type Tail = C

      def apply[A](cc: Nest[Base#Point, C#Point[A]]): (Nest0 -|: C)#Point[A] = cc.asInstanceOf[(Nest0 -|: C)#Point[A]]
    }

    implicit def corecurse[F[_], C <: Effects](implicit C: NestedAtPoint[C]): NestedAtPoint.Aux[F |: C, C.Nest, F |: C.Front, C.Tail] = new NestedAtPoint[F |: C] {
      type Nest[F[_], A] = C.Nest[F, A]
      type Front = F |: C.Front
      type Tail = C.Tail

      def apply[A](cc: Nest[Front#Point, Tail#Point[A]]) = cc.asInstanceOf[(F |: C)#Point[A]]
    }
  }
}

object Effects {
  import Properties._

  @implicitNotFound("could not compute a method for mapping over effect stack ${C}; either a member of the stack lacks an Applicative, or its Applicative instance is ambiguous")
  trait Mapper[C <: Effects] {
    def point[A](a: A): C#Point[A]
    def map[A, B](fa: C#Point[A])(f: A => B): C#Point[B]
  }

  object Mapper {

    implicit def nnmapper[C <: Effects](implicit C: NNMapper[C]): Mapper[C] = new Mapper[C] {
      def point[A](a: A) = C.point(a)
      def map[A, B](fa: C#Point[A])(f: A => B) = C.map(fa)(f)
    }
  }

  trait NNMapper[C <: Effects] {
    type CC[A] = C#Point[A]

    def point[A](a: A): CC[A]

    def map[A, B](fa: CC[A])(f: A => B): CC[B]
  }

  trait MapperLowPriorityImplicits {
    import cats.state.State

    /*implicit def headState[S]: NNMapper[State[S, ?] |: Base] = new NNMapper[State[S, ?] |: Base] {

      def point[A](a: A) = State.pure(a)

      def map[A, B](fa: State[S, A])(f: A => B): State[S, B] = fa map f
    }

    implicit def corecurseState[S, C <: Effects](implicit P: NNMapper[C]): NNMapper[State[S, ?] |: C] = new NNMapper[State[S, ?] |: C] {

      def point[A](a: A) = State.pure(P.point(a))

      def map[A, B](fa: State[S, C#Point[A]])(f: A => B): State[S, C#Point[B]] = fa map { a => P.map(a)(f) }
    }*/
  }

  object NNMapper extends MapperLowPriorityImplicits {

    implicit def head1[F[_]](implicit F: Applicative[F]): NNMapper[F |: Base] = new NNMapper[F |: Base] {

      def point[A](a: A) = F.pure(a)

      def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    }

    implicit def head2[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2], F: Applicative[F2[Z, ?]]): NNMapper[F2[Z, ?] |: Base] = new NNMapper[F2[Z, ?] |: Base] {

      def point[A](a: A) = F.pure(a)

      def map[A, B](fa: F2[Z, A])(f: A => B) = F.map(fa)(f)
    }

    implicit def head3[F[_, _, _], F2[_, _, _], Y, Z](implicit ev: Permute3[F, F2], F: Applicative[F2[Y, Z, ?]]): NNMapper[F2[Y, Z, ?] |: Base] = new NNMapper[F2[Y, Z, ?] |: Base] {

      def point[A](a: A) = F.pure(a)

      def map[A, B](fa: F2[Y, Z, A])(f: A => B) = F.map(fa)(f)
    }

    implicit def headH1[F[_[_], _], G[_]](implicit F: Applicative[F[G, ?]]): NNMapper[F[G, ?] |: Base] = new NNMapper[F[G, ?] |: Base] {

      def point[A](a: A) = F.pure(a)

      def map[A, B](fa: F[G, A])(f: A => B): F[G, B] = F.map(fa)(f)
    }

    implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z](implicit ev: PermuteH2[F, F2], F: Applicative[F2[G, Z, ?]]): NNMapper[F2[G, Z, ?] |: Base] = new NNMapper[F2[G, Z, ?] |: Base] {

      def point[A](a: A) = F.pure(a)

      def map[A, B](fa: F2[G, Z, A])(f: A => B): F2[G, Z, B] = F.map(fa)(f)
    }

    implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z](implicit ev: PermuteH3[F, F2], F: Applicative[F2[G, Y, Z, ?]]): NNMapper[F2[G, Y, Z, ?] |: Base] = new NNMapper[F2[G, Y, Z, ?] |: Base] {

      def point[A](a: A) = F.pure(a)

      def map[A, B](fa: F2[G, Y, Z, A])(f: A => B): F2[G, Y, Z, B] = F.map(fa)(f)
    }

    implicit def corecurse1[F[_], C <: Effects](implicit P: NNMapper[C], NN: NonNested[C], F: Applicative[F]): NNMapper[F |: C] = new NNMapper[F |: C] {

      def point[A](a: A) = NN.pack(F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack(F.map(NN.unpack(fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], P: NNMapper[C], NN: NonNested[C], F: Applicative[F2[Z, ?]]): NNMapper[F2[Z, ?] |: C] = new NNMapper[F2[Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[Z, ?], B](F.map(NN.unpack[F2[Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], P: NNMapper[C], NN: NonNested[C], F: Applicative[F2[Y, Z, ?]]): NNMapper[F2[Y, Z, ?] |: C] = new NNMapper[F2[Y, Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[Y, Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[Y, Z, ?], B](F.map(NN.unpack[F2[Y, Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit P: NNMapper[C], NN: NonNested[C], F: Applicative[F[G, ?]]): NNMapper[F[G, ?] |: C] = new NNMapper[F[G, ?] |: C] {

      def point[A](a: A) = NN.pack[F[G, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F[G, ?], B](F.map(NN.unpack[F[G, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], P: NNMapper[C], NN: NonNested[C], F: Applicative[F2[G, Z, ?]]): NNMapper[F2[G, Z, ?] |: C] = new NNMapper[F2[G, Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[G, Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[G, Z, ?], B](F.map(NN.unpack[F2[G, Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], P: NNMapper[C], NN: NonNested[C], F: Applicative[F2[G, Y, Z, ?]]): NNMapper[F2[G, Y, Z, ?] |: C] = new NNMapper[F2[G, Y, Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[G, Y, Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[G, Y, Z, ?], B](F.map(NN.unpack[F2[G, Y, Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }
  }

  /*trait Traverser[C <: Effects] {
    def traverse[G[_]: Applicative, A, B](ca: C#Point[A])(f: A => G[B]): G[C#Point[B]]
    def foldLeft[A, B](fa: C#Point[A], b: B)(f: (B, A) => B): B
    def foldRight[A, B](fa: C#Point[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
  }

  object Traverser {

    implicit def head1[F[_]](implicit F: Traverse[F]): Traverser[F |: Base] = new Traverser[F |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = F.traverse(fa)(f)
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa, lb)(f)
    }

    implicit def head2[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2], F: Traverse[F2[Z, ?]]): Traverser[F2[Z, ?] |: Base] = new Traverser[F2[Z, ?] |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F2[Z, A])(f: A => G[B]): G[F2[Z, B]] = F.traverse(fa)(f)
      def foldLeft[A, B](fa: F2[Z, A], b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
      def foldRight[A, B](fa: F2[Z, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa, lb)(f)
    }

    implicit def head3[F[_, _, _], F2[_, _, _], Y, Z](implicit ev: Permute3[F, F2], F: Traverse[F2[Y, Z, ?]]): Traverser[F2[Y, Z, ?] |: Base] = new Traverser[F2[Y, Z, ?] |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F2[Y, Z, A])(f: A => G[B]): G[F2[Y, Z, B]] = F.traverse(fa)(f)
      def foldLeft[A, B](fa: F2[Y, Z, A], b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
      def foldRight[A, B](fa: F2[Y, Z, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa, lb)(f)
    }

    implicit def headH1[F[_[_], _], G0[_]](implicit F: Traverse[F[G0, ?]]): Traverser[F[G0, ?] |: Base] = new Traverser[F[G0, ?] |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F[G0, A])(f: A => G[B]): G[F[G0, B]] = F.traverse(fa)(f)
      def foldLeft[A, B](fa: F[G0, A], b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
      def foldRight[A, B](fa: F[G0, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa, lb)(f)
    }

    implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G0[_], Z](implicit ev: PermuteH2[F, F2], F: Traverse[F2[G0, Z, ?]]): Traverser[F2[G0, Z, ?] |: Base] = new Traverser[F2[G0, Z, ?] |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F2[G0, Z, A])(f: A => G[B]): G[F2[G0, Z, B]] = F.traverse(fa)(f)
      def foldLeft[A, B](fa: F2[G0, Z, A], b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
      def foldRight[A, B](fa: F2[G0, Z, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa, lb)(f)
    }

    implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G0[_], Y, Z](implicit ev: PermuteH3[F, F2], F: Traverse[F2[G0, Y, Z, ?]]): Traverser[F2[G0, Y, Z, ?] |: Base] = new Traverser[F2[G0, Y, Z, ?] |: Base] {
      def traverse[G[_]: Applicative, A, B](fa: F2[G0, Y, Z, A])(f: A => G[B]): G[F2[G0, Y, Z, B]] = F.traverse(fa)(f)
      def foldLeft[A, B](fa: F2[G0, Y, Z, A], b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
      def foldRight[A, B](fa: F2[G0, Y, Z, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa, lb)(f)
    }

    implicit def corecurse1[F[_], C <: Effects](implicit C: Traverser[C], F: Traverse[F]): Traverser[F |: C] = new Traverser[F |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[C#Point[A]])(f: A => G[B]): G[F[C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }

      def foldLeft[A, B](fca: F[C#Point[A]], b: B)(f: (B, A) => B): B = {
        F.foldLeft(fca, b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: F[C#Point[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(fca, lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }

    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Traverser[C], F: Traverse[F2[Z, ?]]): Traverser[F2[Z, ?] |: C] = new Traverser[F2[Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F2[Z, C#Point[A]])(f: A => G[B]): G[F2[Z, C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }

      def foldLeft[A, B](fca: F2[Z, C#Point[A]], b: B)(f: (B, A) => B): B = {
        F.foldLeft(fca, b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: F2[Z, C#Point[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(fca, lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Traverser[C], F: Traverse[F2[Y, Z, ?]]): Traverser[F2[Y, Z, ?] |: C] = new Traverser[F2[Y, Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F2[Y, Z, C#Point[A]])(f: A => G[B]): G[F2[Y, Z, C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }

      def foldLeft[A, B](fca: F2[Y, Z, C#Point[A]], b: B)(f: (B, A) => B): B = {
        F.foldLeft(fca, b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: F2[Y, Z, C#Point[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(fca, lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurseH1[F[_[_], _], G0[_], C <: Effects](implicit C: Traverser[C], F: Traverse[F[G0, ?]]): Traverser[F[G0, ?] |: C] = new Traverser[F[G0, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[G0, C#Point[A]])(f: A => G[B]): G[F[G0, C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }

      def foldLeft[A, B](fca: F[G0, C#Point[A]], b: B)(f: (B, A) => B): B = {
        F.foldLeft(fca, b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: F[G0, C#Point[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(fca, lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G0[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Traverser[C], F: Traverse[F[G0, Z, ?]]): Traverser[F[G0, Z, ?] |: C] = new Traverser[F[G0, Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[G0, Z, C#Point[A]])(f: A => G[B]): G[F[G0, Z, C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }

      def foldLeft[A, B](fca: F[G0, Z, C#Point[A]], b: B)(f: (B, A) => B): B = {
        F.foldLeft(fca, b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: F[G0, Z, C#Point[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(fca, lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G0[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Traverser[C], F: Traverse[F[G0, Y, Z, ?]]): Traverser[F[G0, Y, Z, ?] |: C] = new Traverser[F[G0, Y, Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: F[G0, Y, Z, C#Point[A]])(f: A => G[B]): G[F[G0, Y, Z, C#Point[B]]] = {
        F.traverse(fca) { ca =>
          C.traverse(ca)(f)
        }
      }

      def foldLeft[A, B](fca: F[G0, Y, Z, C#Point[A]], b: B)(f: (B, A) => B): B = {
        F.foldLeft(fca, b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: F[G0, Y, Z, C#Point[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(fca, lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }
  }

  @implicitNotFound("could not prove ${C} is a valid monadic stack; perhaps an effect is lacking a FlatMap, or a non-outer effect is lacking a Traverse")
  trait Binder[C <: Effects] {
    def bind[A, B](cca: C#Point[A])(f: A => C#Point[B]): C#Point[B]
  }

  trait BinderLowPriorityImplicits {
    import cats.state.State

    implicit def headState[S]: Binder[State[S, ?] |: Base] = new Binder[State[S, ?] |: Base] {
      def bind[A, B](fa: State[S, A])(f: A => State[S, B]) = fa flatMap f
    }

    implicit def corecurseState[S, C <: Effects](implicit C: Binder[C], T: Traverser[C]): Binder[State[S, ?] |: C] = new Binder[State[S, ?] |: C] {

      def bind[A, B](fa: State[S, C#Point[A]])(f: A => State[S, C#Point[B]]): State[S, C#Point[B]] = {
        fa flatMap { ca =>
          val scca = T.traverse[State[S, ?], A, C#Point[B]](ca) { a => f(a) }

          scca map { cca => C.bind(cca) { a => a } }
        }
      }
    }
  }

  object Binder extends BinderLowPriorityImplicits {

    implicit def head1[F[_]](implicit F: FlatMap[F]): Binder[F |: Base] = new Binder[F |: Base] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
    }

    implicit def head2[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2], F: FlatMap[F2[Z, ?]]): Binder[F2[Z, ?] |: Base] = new Binder[F2[Z, ?] |: Base] {
      def bind[A, B](fa: F2[Z, A])(f: A => F2[Z, B]) = F.flatMap(fa)(f)
    }

    implicit def head3[F[_, _, _], F2[_, _, _], Y, Z](implicit ev: Permute3[F, F2], F: FlatMap[F2[Y, Z, ?]]): Binder[F2[Y, Z, ?] |: Base] = new Binder[F2[Y, Z, ?] |: Base] {
      def bind[A, B](fa: F2[Y, Z, A])(f: A => F2[Y, Z, B]) = F.flatMap(fa)(f)
    }

    implicit def headH1[F[_[_], _], G[_]](implicit F: FlatMap[F[G, ?]]): Binder[F[G, ?] |: Base] = new Binder[F[G, ?] |: Base] {
      def bind[A, B](fa: F[G, A])(f: A => F[G, B]): F[G, B] = F.flatMap(fa)(f)
    }

    implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z](implicit ev: PermuteH2[F, F2], F: FlatMap[F2[G, Z, ?]]): Binder[F2[G, Z, ?] |: Base] = new Binder[F2[G, Z, ?] |: Base] {
      def bind[A, B](fa: F2[G, Z, A])(f: A => F2[G, Z, B]): F2[G, Z, B] = F.flatMap(fa)(f)
    }

    implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z](implicit ev: PermuteH3[F, F2], F: FlatMap[F2[G, Y, Z, ?]]): Binder[F2[G, Y, Z, ?] |: Base] = new Binder[F2[G, Y, Z, ?] |: Base] {
      def bind[A, B](fa: F2[G, Y, Z, A])(f: A => F2[G, Y, Z, B]): F2[G, Y, Z, B] = F.flatMap(fa)(f)
    }

    implicit def corecurse1[F[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], F: Applicative[F], B: FlatMap[F]): Binder[F |: C] = new Binder[F |: C] {

      def bind[A, B](fca: F[C#Point[A]])(f: A => F[C#Point[B]]): F[C#Point[B]] = {
        B.flatMap(fca) { ca =>
          val fcaca = T.traverse(ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Binder[C], T: Traverser[C], F: Applicative[F2[Z, ?]], B: FlatMap[F2[Z, ?]]): Binder[F2[Z, ?] |: C] = new Binder[F2[Z, ?] |: C] {

      def bind[A, B](fca: F2[Z, C#Point[A]])(f: A => F2[Z, C#Point[B]]): F2[Z, C#Point[B]] = {
        B.flatMap(fca) { ca =>
          val fcaca = T.traverse[F2[Z, ?], A, C#Point[B]](ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Binder[C], T: Traverser[C], F: Applicative[F2[Y, Z, ?]], B: FlatMap[F2[Y, Z, ?]]): Binder[F2[Y, Z, ?] |: C] = new Binder[F2[Y, Z, ?] |: C] {

      def bind[A, B](fca: F2[Y, Z, C#Point[A]])(f: A => F2[Y, Z, C#Point[B]]): F2[Y, Z, C#Point[B]] = {
        B.flatMap(fca) { ca =>
          val fcaca = T.traverse[F2[Y, Z, ?], A, C#Point[B]](ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }

    implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], F: Applicative[F[G, ?]], B: FlatMap[F[G, ?]]): Binder[F[G, ?] |: C] = new Binder[F[G, ?] |: C] {

      def bind[A, B](fca: F[G, C#Point[A]])(f: A => F[G, C#Point[B]]): F[G, C#Point[B]] = {
        B.flatMap(fca) { ca =>
          val fcaca = T.traverse[F[G, ?], A, C#Point[B]](ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Binder[C], T: Traverser[C], F: Applicative[F2[G, Z, ?]], B: FlatMap[F2[G, Z, ?]]): Binder[F2[G, Z, ?] |: C] = new Binder[F2[G, Z, ?] |: C] {

      def bind[A, B](fca: F2[G, Z, C#Point[A]])(f: A => F2[G, Z, C#Point[B]]): F2[G, Z, C#Point[B]] = {
        B.flatMap(fca) { ca =>
          val fcaca = T.traverse[F2[G, Z, ?], A, C#Point[B]](ca) { a => f(a) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }
      }
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Binder[C], T: Traverser[C], F: Applicative[F2[G, Y, Z, ?]], B: FlatMap[F2[G, Y, Z, ?]]): Binder[F2[G, Y, Z, ?] |: C] = new Binder[F2[G, Y, Z, ?] |: C] {

      def bind[A, B](fca: F2[G, Y, Z, C#Point[A]])(f: A => F2[G, Y, Z, C#Point[B]]): F2[G, Y, Z, C#Point[B]] = {
        B.flatMap(fca) { ca =>
          val fcaca = T.traverse[F2[G, Y, Z, ?], A, C#Point[B]](ca) { a => f(a) }

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

  trait ExpanderLowPriorityImplicits {
    import cats.state.State

    implicit def headState[S]: Expander.Aux[State[S, ?] |: Base, State[S, ?], Base] = new Expander[State[S, ?] |: Base] {
      type CC[A] = State[S, A]
      type Out = Base

      def apply[A](fa: State[S, A]): State[S, A] = fa
    }

    implicit def corecurseState[S, C <: Effects](implicit C: Expander[C]): Expander.Aux[State[S, ?] |: C, C.CC, State[S, ?] |: C.Out] = new Expander[State[S, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = State[S, ?] |: C.Out

      def apply[A](gca: State[S, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }
  }

  object Expander extends ExpanderLowPriorityImplicits {
    type Aux[C <: Effects, CC0[_], Out0 <: Effects] = Expander[C] { type CC[A] = CC0[A]; type Out = Out0 }

    implicit def head1[F[_]]: Expander.Aux[F |: Base, F, Base] = new Expander[F |: Base] {
      type CC[A] = F[A]
      type Out = Base

      def apply[A](fa: F[A]): F[A] = fa
    }

    implicit def head2[F[_, _], F2[_, _], Z](implicit ev: Permute2[F, F2]): Expander.Aux[F2[Z, ?] |: Base, F2[Z, ?], Base] = new Expander[F2[Z, ?] |: Base] {
      type CC[A] = F2[Z, A]
      type Out = Base

      def apply[A](fa: F2[Z, A]): F2[Z, A] = fa
    }

    implicit def head3[F[_, _, _], F2[_, _, _], Y, Z](implicit ev: Permute3[F, F2]): Expander.Aux[F2[Y, Z, ?] |: Base, F2[Y, Z, ?], Base] = new Expander[F2[Y, Z, ?] |: Base] {
      type CC[A] = F2[Y, Z, A]
      type Out = Base

      def apply[A](fa: F2[Y, Z, A]): F2[Y, Z, A] = fa
    }

    implicit def headH1[F[_[_], _], G[_]]: Expander.Aux[F[G, ?] |: Base, F[G, ?], Base] = new Expander[F[G, ?] |: Base] {
      type CC[A] = F[G, A]
      type Out = Base

      def apply[A](fa: F[G, A]): F[G, A] = fa
    }

    implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z](implicit ev: PermuteH2[F, F2]): Expander.Aux[F2[G, Z, ?] |: Base, F2[G, Z, ?], Base] = new Expander[F2[G, Z, ?] |: Base] {
      type CC[A] = F2[G, Z, A]
      type Out = Base

      def apply[A](fa: F2[G, Z, A]): F2[G, Z, A] = fa
    }

    implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z](implicit ev: PermuteH3[F, F2]): Expander.Aux[F2[G, Y, Z, ?] |: Base, F2[G, Y, Z, ?], Base] = new Expander[F2[G, Y, Z, ?] |: Base] {
      type CC[A] = F2[G, Y, Z, A]
      type Out = Base

      def apply[A](fa: F2[G, Y, Z, A]): F2[G, Y, Z, A] = fa
    }

    implicit def corecurse1[F[_], C <: Effects](implicit C: Expander[C]): Expander.Aux[F |: C, C.CC, F |: C.Out] = new Expander[F |: C] {
      type CC[A] = C.CC[A]
      type Out = F |: C.Out

      def apply[A](gca: F[C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Expander[C]): Expander.Aux[F2[Z, ?] |: C, C.CC, F2[Z, ?] |: C.Out] = new Expander[F2[Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[Z, ?] |: C.Out

      def apply[A](gca: F2[Z, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Expander[C]): Expander.Aux[F2[Y, Z, ?] |: C, C.CC, F2[Y, Z, ?] |: C.Out] = new Expander[F2[Y, Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[Y, Z, ?] |: C.Out

      def apply[A](gca: F2[Y, Z, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: Expander[C]): Expander.Aux[F[G, ?] |: C, C.CC, F[G, ?] |: C.Out] = new Expander[F[G, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F[G, ?] |: C.Out

      def apply[A](gca: F[G, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Expander[C]): Expander.Aux[F2[G, Z, ?] |: C, C.CC, F2[G, Z, ?] |: C.Out] = new Expander[F2[G, Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[G, Z, ?] |: C.Out

      def apply[A](gca: F2[G, Z, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Expander[C]): Expander.Aux[F2[G, Y, Z, ?] |: C, C.CC, F2[G, Y, Z, ?] |: C.Out] = new Expander[F2[G, Y, Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[G, Y, Z, ?] |: C.Out

      def apply[A](gca: F2[G, Y, Z, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }
  }

  trait Collapser[E, C <: Effects] {
    type A
    type Out <: Effects

    def apply(fa: C#Point[E]): Out#Point[A]
  }

  trait CollapserLowPriorityImplicits1 {
    import cats.state.State

    implicit def headState[S, A0]: Collapser.Aux[State[S, A0], Base, A0, State[S, ?] |: Base] = new Collapser[State[S, A0], Base] {
      type A = A0
      type Out = State[S, ?] |: Base

      def apply(fa: State[S, A]): State[S, A] = fa
    }

    implicit def corecurseState[E, S, C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, State[S, ?] |: C, C.A, State[S, ?] |: C.Out] = new Collapser[E, State[S, ?] |: C] {
      type A = C.A
      type Out = State[S, ?] |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: State[S, C#Point[E]]): State[S, C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }
  }

  trait CollapserLowPriorityImplicits2 extends CollapserLowPriorityImplicits1 {

    implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, A0](implicit ev: PermuteH2[F, F2]): Collapser.Aux[F2[G, Z, A0], Base, A0, F2[G, Z, ?] |: Base] = new Collapser[F2[G, Z, A0], Base] {
      type A = A0
      type Out = F2[G, Z, ?] |: Base

      def apply(fa: F2[G, Z, A]): F2[G, Z, A] = fa
    }

    implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, A0](implicit ev: PermuteH3[F, F2]): Collapser.Aux[F2[G, Y, Z, A0], Base, A0, F2[G, Y, Z, ?] |: Base] = new Collapser[F2[G, Y, Z, A0], Base] {
      type A = A0
      type Out = F2[G, Y, Z, ?] |: Base

      def apply(fa: F2[G, Y, Z, A]): F2[G, Y, Z, A] = fa
    }

    implicit def corecurseH2[E, F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[G, Z, ?] |: C, C.A, F2[G, Z, ?] |: C.Out] = new Collapser[E, F2[G, Z, ?] |: C] {
      type A = C.A
      type Out = F2[G, Z, ?] |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F2[G, Z, C#Point[E]]): F2[G, Z, C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH3[E, F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[G, Y, Z, ?] |: C, C.A, F2[G, Y, Z, ?] |: C.Out] = new Collapser[E, F2[G, Y, Z, ?] |: C] {
      type A = C.A
      type Out = F2[G, Y, Z, ?] |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F2[G, Y, Z, C#Point[E]]): F2[G, Y, Z, C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }
  }

  object Collapser extends CollapserLowPriorityImplicits2 {
    type Aux[E, C <: Effects, A0, Out0 <: Effects] = Collapser[E, C] { type A = A0; type Out = Out0 }

    implicit def head1[F[_], A0]: Collapser.Aux[F[A0], Base, A0, F |: Base] = new Collapser[F[A0], Base] {
      type A = A0
      type Out = F |: Base

      def apply(fa: F[A]): F[A] = fa
    }

    implicit def head2[F[_, _], F2[_, _], Z, A0](implicit ev: Permute2[F, F2]): Collapser.Aux[F2[Z, A0], Base, A0, F2[Z, ?] |: Base] = new Collapser[F2[Z, A0], Base] {
      type A = A0
      type Out = F2[Z, ?] |: Base

      def apply(fa: F2[Z, A]): F2[Z, A] = fa
    }

    implicit def head3[F[_, _, _], F2[_, _, _], Y, Z, A0](implicit ev: Permute3[F, F2]): Collapser.Aux[F2[Y, Z, A0], Base, A0, F2[Y, Z, ?] |: Base] = new Collapser[F2[Y, Z, A0], Base] {
      type A = A0
      type Out = F2[Y, Z, ?] |: Base

      def apply(fa: F2[Y, Z, A]): F2[Y, Z, A] = fa
    }

    implicit def headH1[F[_[_], _], G[_], A0]: Collapser.Aux[F[G, A0], Base, A0, F[G, ?] |: Base] = new Collapser[F[G, A0], Base] {
      type A = A0
      type Out = F[G, ?] |: Base

      def apply(fa: F[G, A]): F[G, A] = fa
    }

    implicit def corecurse1[E, F[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F |: C, C.A, F |: C.Out] = new Collapser[E, F |: C] {
      type A = C.A
      type Out = F |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F[C#Point[E]]): F[C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse2[E, F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[Z, ?] |: C, C.A, F2[Z, ?] |: C.Out] = new Collapser[E, F2[Z, ?] |: C] {
      type A = C.A
      type Out = F2[Z, ?] |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F2[Z, C#Point[E]]): F2[Z, C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse3[E, F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[Y, Z, ?] |: C, C.A, F2[Y, Z, ?] |: C.Out] = new Collapser[E, F2[Y, Z, ?] |: C] {
      type A = C.A
      type Out = F2[Y, Z, ?] |: C.Out

      // if I use the aliases, scalac gets very confused...
      def apply(gca: F2[Y, Z, C#Point[E]]): F2[Y, Z, C.Out#Point[C.A]] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH1[E, F[_[_], _], G[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F[G, ?] |: C, C.A, F[G, ?] |: C.Out] = new Collapser[E, F[G, ?] |: C] {
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

  trait LifterLowPriorityImplicits {
    import cats.state.State

    implicit def headState[S, A]: Lifter.Aux[State[S, A], State[S, ?] |: Base, A] = new Lifter[State[S, A], State[S, ?] |: Base] {
      type Out = A

      def apply(fa: State[S, A]) = fa
    }

    implicit def midState[S, A, C <: Effects](implicit C: NNMapper[C]): Lifter.Aux[State[S, A], State[S, ?] |: C, A] = new Lifter[State[S, A], State[S, ?] |: C] {
      type Out = A

      def apply(fa: State[S, A]) = fa map { a => C.point(a) }
    }

    implicit def corecurseState[S, E, C <: Effects](implicit L: Lifter[E, C]): Lifter.Aux[E, State[S, ?] |: C, L.Out] = new Lifter[E, State[S, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = State.pure(L(e))
    }
  }

  object Lifter extends LifterLowPriorityImplicits {
    type Aux[E, C <: Effects, Out0] = Lifter[E, C] { type Out = Out0 }

    implicit def head1[F[_], A]: Lifter.Aux[F[A], F |: Base, A] = new Lifter[F[A], F |: Base] {
      type Out = A

      def apply(fa: F[A]) = fa
    }

    implicit def head2[F[_, _], F2[_, _], Z, A](implicit ev: Permute2[F, F2]): Lifter.Aux[F2[Z, A], F2[Z, ?] |: Base, A] = new Lifter[F2[Z, A], F2[Z, ?] |: Base] {
      type Out = A

      def apply(fa: F2[Z, A]) = fa
    }

    implicit def head3[F[_, _, _], F2[_, _, _], Y, Z, A](implicit ev: Permute3[F, F2]): Lifter.Aux[F2[Y, Z, A], F2[Y, Z, ?] |: Base, A] = new Lifter[F2[Y, Z, A], F2[Y, Z, ?] |: Base] {
      type Out = A

      def apply(fa: F2[Y, Z, A]) = fa
    }

    implicit def headH1[F[_[_], _], G[_], A]: Lifter.Aux[F[G, A], F[G, ?] |: Base, A] = new Lifter[F[G, A], F[G, ?] |: Base] {
      type Out = A

      def apply(fa: F[G, A]) = fa
    }

    implicit def headH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, A](implicit ev: PermuteH2[F, F2]): Lifter.Aux[F2[G, Z, A], F2[G, Z, ?] |: Base, A] = new Lifter[F2[G, Z, A], F2[G, Z, ?] |: Base] {
      type Out = A

      def apply(fa: F2[G, Z, A]) = fa
    }

    implicit def headH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, A](implicit ev: PermuteH3[F, F2]): Lifter.Aux[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: Base, A] = new Lifter[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: Base] {
      type Out = A

      def apply(fa: F2[G, Y, Z, A]) = fa
    }

    implicit def mid1[F[_], A, C <: Effects](implicit C: NNMapper[C], F: Functor[F]): Lifter.Aux[F[A], F |: C, A] = new Lifter[F[A], F |: C] {
      type Out = A

      def apply(fa: F[A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def mid2[F[_, _], F2[_, _], Z, A, C <: Effects](implicit ev: Permute2[F, F2], C: NNMapper[C], F: Functor[F2[Z, ?]]): Lifter.Aux[F2[Z, A], F2[Z, ?] |: C, A] = new Lifter[F2[Z, A], F2[Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[Z, A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def mid3[F[_, _, _], F2[_, _, _], Y, Z, A, C <: Effects](implicit ev: Permute3[F, F2], C: NNMapper[C], F: Functor[F2[Y, Z, ?]]): Lifter.Aux[F2[Y, Z, A], F2[Y, Z, ?] |: C, A] = new Lifter[F2[Y, Z, A], F2[Y, Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[Y, Z, A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def midH1[F[_[_], _], G[_], A, C <: Effects](implicit C: NNMapper[C], F: Functor[F[G, ?]]): Lifter.Aux[F[G, A], F[G, ?] |: C, A] = new Lifter[F[G, A], F[G, ?] |: C] {
      type Out = A

      def apply(fa: F[G, A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def midH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, A, C <: Effects](implicit ev: PermuteH2[F, F2], C: NNMapper[C], F: Functor[F2[G, Z, ?]]): Lifter.Aux[F2[G, Z, A], F2[G, Z, ?] |: C, A] = new Lifter[F2[G, Z, A], F2[G, Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[G, Z, A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def midH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, A, C <: Effects](implicit ev: PermuteH3[F, F2], C: NNMapper[C], F: Functor[F2[G, Y, Z, ?]]): Lifter.Aux[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: C, A] = new Lifter[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[G, Y, Z, A]) = F.map(fa) { a => C.point(a) }
    }

    implicit def corecurse1[F[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F]): Lifter.Aux[E, F |: C, L.Out] = new Lifter[E, F |: C] {
      type Out = L.Out

      def apply(e: E) = F.pure(L(e))
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects](implicit ev: Permute2[F, F2], L: Lifter[E, C], F: Applicative[F2[Z, ?]]): Lifter.Aux[E, F2[Z, ?] |: C, L.Out] = new Lifter[E, F2[Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = F.pure(L(e))
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, E, C <: Effects](implicit ev: Permute3[F, F2], L: Lifter[E, C], F: Applicative[F2[Y, Z, ?]]): Lifter.Aux[E, F2[Y, Z, ?] |: C, L.Out] = new Lifter[E, F2[Y, Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = F.pure(L(e))
    }

    implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F[G, ?]]): Lifter.Aux[E, F[G, ?] |: C, L.Out] = new Lifter[E, F[G, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = F.pure(L(e))
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, E, C <: Effects](implicit ev: PermuteH2[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Z, ?]]): Lifter.Aux[E, F2[G, Z, ?] |: C, L.Out] = new Lifter[E, F2[G, Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = F.pure(L(e))
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, E, C <: Effects](implicit ev: PermuteH3[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Y, Z, ?]]): Lifter.Aux[E, F2[G, Y, Z, ?] |: C, L.Out] = new Lifter[E, F2[G, Y, Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = F.pure(L(e))
    }
  }

  @implicitNotFound("could not infer effect stack ${C} from type ${E}")
  trait Wrapper[E, C <: Effects] {
    type A

    def apply(e: E): C#Point[A]
  }

  trait WrapperLowPriorityImplicits1 {
    import cats.state.State

    implicit def head[A0]: Wrapper.Aux[A0, Base, A0] = new Wrapper[A0, Base] {
      type A = A0

      def apply(a: A) = a
    }

    // state's definition in scalaz is weird enough to confuse scalac, but it's an important effect to support
    implicit def corecurseState[S, E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[State[S, E], State[S, ?] |: C, A0] = new Wrapper[State[S, E], State[S, ?] |: C] {
      type A = A0

      def apply(s: State[S, E]): State[S, C#Point[A0]] = s map { e => W(e) }
    }
  }

  // not really sure why these functions in particular need to be moved down
  trait WrapperLowPriorityImplicits2 extends WrapperLowPriorityImplicits1 {

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, E, C <: Effects, A0](implicit ev: PermuteH2[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[G, Z, E], F2[G, Z, ?] |: C, A0] = new Wrapper[F2[G, Z, E], F2[G, Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[G, Z, E]): F2[G, Z, C#Point[A]] =
        fe.asInstanceOf[F2[G, Z, C#Point[A]]]      // already proven equivalent; actual evaluation requires a Functor
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, E, C <: Effects, A0](implicit ev: PermuteH3[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[G, Y, Z, E], F2[G, Y, Z, ?] |: C, A0] = new Wrapper[F2[G, Y, Z, E], F2[G, Y, Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[G, Y, Z, E]): F2[G, Y, Z, C#Point[A]] =
        fe.asInstanceOf[F2[G, Y, Z, C#Point[A]]]      // already proven equivalent; actual evaluation requires a Functor
    }
  }

  object Wrapper extends WrapperLowPriorityImplicits2 {
    type Aux[E, C <: Effects, A0] = Wrapper[E, C] { type A = A0 }

    implicit def corecurse1[F[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[E], F |: C, A0] = new Wrapper[F[E], F |: C] {
      type A = A0

      def apply(fe: F[E]): F[C#Point[A]] =
        fe.asInstanceOf[F[C#Point[A]]]      // already proven equivalent; actual evaluation requires a Functor
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects, A0](implicit ev: Permute2[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[Z, E], F2[Z, ?] |: C, A0] = new Wrapper[F2[Z, E], F2[Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[Z, E]): F2[Z, C#Point[A]] =
        fe.asInstanceOf[F2[Z, C#Point[A]]]
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, E, C <: Effects, A0](implicit ev: Permute3[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[Y, Z, E], F2[Y, Z, ?] |: C, A0] = new Wrapper[F2[Y, Z, E], F2[Y, Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[Y, Z, E]): F2[Y, Z, C#Point[A]] =
        fe.asInstanceOf[F2[Y, Z, C#Point[A]]]
    }

    implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[G, E], F[G, ?] |: C, A0] = new Wrapper[F[G, E], F[G, ?] |: C] {
      type A = A0

      def apply(fe: F[G, E]): F[G, C#Point[A]] =
        fe.asInstanceOf[F[G, C#Point[A]]]      // already proven equivalent; actual evaluation requires a Functor
    }
  }*/
}


final case class Emm[C <: Effects, A](run: C#Point[A]) {
  import Effects._

  def map[B](f: A => B)(implicit C: NNMapper[C]): Emm[C, B] = Emm(C.map(run)(f))

/*  def flatMap[B](f: A => Emm[C, B])(implicit B: Binder[C]): Emm[C, B] =
    Emm(B.bind(run) { a => f(a).run })

  def flatMapM[E](f: A => E)(implicit E: Lifter[E, C], B: Binder[C]): Emm[C, E.Out] =
    flatMap { a => Emm(E(f(a))) }

  def expand(implicit C: Expander[C]): Emm[C.Out, C.CC[A]] = Emm(C(run))

  def collapse(implicit C: Collapser[A, C]): Emm[C.Out, C.A] = Emm(C(run))*/
}

/*trait EmmLowPriorityImplicits1 {
  import Effects._

  implicit def functorInstance[C <: Effects](implicit C: NNMapper[C]): Functor[Emm[C, ?]] = new Functor[Emm[C, ?]] {

    def map[A, B](fa: Emm[C, A])(f: A => B): Emm[C, B] = new Emm(C.map(fa.run)(f))
  }
}

trait EmmLowPriorityImplicits2 extends EmmLowPriorityImplicits1 {
  import Effects._

  implicit def monadInstance[C <: Effects : NNMapper : Binder]: Monad[Emm[C, ?]] = new Monad[Emm[C, ?]] {

    def pure[A](a: A): Emm[C, A] = new Emm(implicitly[NNMapper[C]].point(a))

    def flatMap[A, B](fa: Emm[C, A])(f: A => Emm[C, B]): Emm[C, B] = fa flatMap f
  }
}

object Emm extends EmmLowPriorityImplicits2 {
  import Effects._

  implicit def traverseInstance[C <: Effects](implicit C: Traverser[C]): Traverse[Emm[C, ?]] = new Traverse[Emm[C, ?]] {
    def traverse[G[_]: Applicative, A, B](fa: Emm[C, A])(f: A => G[B]): G[Emm[C, B]] =
      Applicative[G].map(C.traverse(fa.run)(f)) { new Emm(_) }

    def foldLeft[A, B](fa: Emm[C, A], b: B)(f: (B, A) => B): B = C.foldLeft(fa.run, b)(f)

    def foldRight[A, B](fa: Emm[C, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = C.foldRight(fa.run, lb)(f)

  }
}*/