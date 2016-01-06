package emm

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}

import scala.annotation.implicitNotFound

// there's a bug in scalac's cyclic checking with open recursion and type constructors with arity > 1
trait Partial {
  type Apply[F[_]]
}

sealed trait Effects {
  type Point[A] = Build[λ[X => X], A]

  type Build[CC[_], A] = Inner[A]#Apply[CC]

  type Inner[A] <: Partial
}

sealed trait |:[F[_], T <: Effects] extends Effects {
  type Inner[A] = Partial { type Apply[CC[_]] = T#Inner[A]#Apply[λ[X => CC[F[X]]]] }
}

sealed trait -|:[F[_[_], _], T <: Effects] extends Effects {
  type Inner[A] = Partial { type Apply[CC[_]] = F[CC, T#Inner[A]#Apply[λ[X => X]]] }
}

sealed trait Base extends Effects {
  type Inner[A] = Partial { type Apply[F[_]] = F[A] }
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

    implicit def base: NonNested[Base] = new NonNested[Base] {
      def pack[CC[_], A](cc: CC[A]) = cc
      def unpack[CC[_], A](cc: CC[A]) = cc
    }

    implicit def corecurse1[F[_], C <: Effects](implicit C: NonNested[C]): NonNested[F |: C] = new NonNested[F |: C] {
      def pack[CC[_], A](cc: CC[(F |: C)#Point[A]]) = cc.asInstanceOf[(F |: C)#Build[CC, A]]
      def unpack[CC[_], A](cc: (F |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F |: C)#Point[A]]]
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: NonNested[C]): NonNested[F2[Z, ?] |: C] = new NonNested[F2[Z, ?] |: C] {
      def pack[CC[_], A](cc: CC[(F2[Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[Z, ?] |: C)#Build[CC, A]]
      def unpack[CC[_], A](cc: (F2[Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[Z, ?] |: C)#Point[A]]]
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: NonNested[C]): NonNested[F2[Y, Z, ?] |: C] = new NonNested[F2[Y, Z, ?] |: C] {
      def pack[CC[_], A](cc: CC[(F2[Y, Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[Y, Z, ?] |: C)#Build[CC, A]]
      def unpack[CC[_], A](cc: (F2[Y, Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[Y, Z, ?] |: C)#Point[A]]]
    }

    implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: NonNested[C]): NonNested[F[G, ?] |: C] = new NonNested[F[G, ?] |: C] {
      def pack[CC[_], A](cc: CC[(F[G, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F[G, ?] |: C)#Build[CC, A]]
      def unpack[CC[_], A](cc: (F[G, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F[G, ?] |: C)#Point[A]]]
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: NonNested[C]): NonNested[F2[G, Z, ?] |: C] = new NonNested[F2[G, Z, ?] |: C] {
      def pack[CC[_], A](cc: CC[(F2[G, Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[G, Z, ?] |: C)#Build[CC, A]]
      def unpack[CC[_], A](cc: (F2[G, Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[G, Z, ?] |: C)#Point[A]]]
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: NonNested[C]): NonNested[F2[G, Y, Z, ?] |: C] = new NonNested[F2[G, Y, Z, ?] |: C] {
      def pack[CC[_], A](cc: CC[(F2[G, Y, Z, ?] |: C)#Point[A]]) = cc.asInstanceOf[(F2[G, Y, Z, ?] |: C)#Build[CC, A]]
      def unpack[CC[_], A](cc: (F2[G, Y, Z, ?] |: C)#Build[CC, A]) = cc.asInstanceOf[CC[(F2[G, Y, Z, ?] |: C)#Point[A]]]
    }
  }

  /**
   * The property of Effects which contain at least one -|: case, partitioning into a front and tail, where the tail
   * is NonNested and the front is unconstrained.
   */
  sealed trait NestedAtPoint[C <: Effects, Pivot[_[_], _], Front <: Effects, Tail <: Effects] {
    def NN: NonNested[Tail]

    def pack[A](cc: Pivot[Front#Point, Tail#Point[A]]): C#Point[A]
    def unpack[A](cc: C#Point[A]): Pivot[Front#Point, Tail#Point[A]]
  }

  object NestedAtPoint {

    implicit def split1[Pivot[_[_], _], C <: Effects](implicit C: NonNested[C]): NestedAtPoint[Pivot -|: C, Pivot, Base, C] = new NestedAtPoint[Pivot -|: C, Pivot, Base, C] {
      def NN = C

      def pack[A](cc: Pivot[λ[X => X], C#Point[A]]): (Pivot -|: C)#Point[A] = cc.asInstanceOf[(Pivot -|: C)#Point[A]]
      def unpack[A](cc: (Pivot -|: C)#Point[A]): Pivot[λ[X => X], C#Point[A]] = cc.asInstanceOf[Pivot[λ[X => X], C#Point[A]]]
    }

    implicit def split2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects](implicit ev: PermuteH2[Pivot, Pivot2], C: NonNested[C]): NestedAtPoint[Pivot2[?[_], Z, ?] -|: C, Pivot2[?[_], Z, ?], Base, C] = new NestedAtPoint[Pivot2[?[_], Z, ?] -|: C, Pivot2[?[_], Z, ?], Base, C] {
      def NN = C

      def pack[A](cc: Pivot2[λ[X => X], Z, C#Point[A]]): (Pivot2[?[_], Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(Pivot2[?[_], Z, ?] -|: C)#Point[A]]
      def unpack[A](cc: (Pivot2[?[_], Z, ?] -|: C)#Point[A]): Pivot2[λ[X => X], Z, C#Point[A]] = cc.asInstanceOf[Pivot2[λ[X => X], Z, C#Point[A]]]
    }

    implicit def split3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects](implicit ev: PermuteH3[Pivot, Pivot2], C: NonNested[C]): NestedAtPoint[Pivot2[?[_], Y, Z, ?] -|: C, Pivot2[?[_], Y, Z, ?], Base, C] = new NestedAtPoint[Pivot2[?[_], Y, Z, ?] -|: C, Pivot2[?[_], Y, Z, ?], Base, C] {
      def NN = C

      def pack[A](cc: Pivot2[λ[X => X], Y, Z, C#Point[A]]): (Pivot2[?[_], Y, Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(Pivot2[?[_], Y, Z, ?] -|: C)#Point[A]]
      def unpack[A](cc: (Pivot2[?[_], Y, Z, ?] -|: C)#Point[A]): Pivot2[λ[X => X], Y, Z, C#Point[A]] = cc.asInstanceOf[Pivot2[λ[X => X], Y, Z, C#Point[A]]]
    }

    // TODO do I have to multiply all of these cases by various Pivot permutations?  need to test (e.g. StateT)
    implicit def corecurseBar1[G[_], Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G |: C, Pivot, G |: F, T] = new NestedAtPoint[G |: C, Pivot, G |: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G |: F)#Point, T#Point[A]]): (G |: C)#Point[A] = cc.asInstanceOf[(G |: C)#Point[A]]
      def unpack[A](cc: (G |: C)#Point[A]): Pivot[(G |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G |: F)#Point, T#Point[A]]]
    }

    implicit def corecurseBar2[G[_, _], G2[_, _], Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: Permute2[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[Z, ?] |: C, Pivot, G2[Z, ?] |: F, T] = new NestedAtPoint[G2[Z, ?] |: C, Pivot, G2[Z, ?] |: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G2[Z, ?] |: F)#Point, T#Point[A]]): (G2[Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[Z, ?] |: C)#Point[A]]
      def unpack[A](cc: (G2[Z, ?] |: C)#Point[A]): Pivot[(G2[Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[Z, ?] |: F)#Point, T#Point[A]]]
    }

    implicit def corecurseBar3[G[_, _, _], G2[_, _, _], Y, Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: Permute3[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[Y, Z, ?] |: C, Pivot, G2[Y, Z, ?] |: F, T] = new NestedAtPoint[G2[Y, Z, ?] |: C, Pivot, G2[Y, Z, ?] |: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G2[Y, Z, ?] |: F)#Point, T#Point[A]]): (G2[Y, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[Y, Z, ?] |: C)#Point[A]]
      def unpack[A](cc: (G2[Y, Z, ?] |: C)#Point[A]): Pivot[(G2[Y, Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[Y, Z, ?] |: F)#Point, T#Point[A]]]
    }

    implicit def corecurseBarH1[G[_[_], _], H[_], Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G[H, ?] |: C, Pivot, G[H, ?] |: F, T] = new NestedAtPoint[G[H, ?] |: C, Pivot, G[H, ?] |: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G[H, ?] |: F)#Point, T#Point[A]]): (G[H, ?] |: C)#Point[A] = cc.asInstanceOf[(G[H, ?] |: C)#Point[A]]
      def unpack[A](cc: (G[H, ?] |: C)#Point[A]): Pivot[(G[H, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G[H, ?] |: F)#Point, T#Point[A]]]
    }

    implicit def corecurseBarH2[G[_[_], _, _], G2[_[_], _, _], H[_], Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[H, Z, ?] |: C, Pivot, G2[H, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Z, ?] |: C, Pivot, G2[H, Z, ?] |: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G2[H, Z, ?] |: F)#Point, T#Point[A]]): (G2[H, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Z, ?] |: C)#Point[A]]
      def unpack[A](cc: (G2[H, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Z, ?] |: F)#Point, T#Point[A]]]
    }

    implicit def corecurseBarH3[G[_[_], _, _, _], G2[_[_], _, _, _], H[_], Y, Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot, G2[H, Y, Z, ?] |: F, T] = new NestedAtPoint[G2[H, Y, Z, ?] |: C, Pivot, G2[H, Y, Z, ?] |: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G2[H, Y, Z, ?] |: F)#Point, T#Point[A]]): (G2[H, Y, Z, ?] |: C)#Point[A] = cc.asInstanceOf[(G2[H, Y, Z, ?] |: C)#Point[A]]
      def unpack[A](cc: (G2[H, Y, Z, ?] |: C)#Point[A]): Pivot[(G2[H, Y, Z, ?] |: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[H, Y, Z, ?] |: F)#Point, T#Point[A]]]
    }

    implicit def corecursePivot1[G[_[_], _], Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G -|: C, Pivot, G -|: F, T] = new NestedAtPoint[G -|: C, Pivot, G -|: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G -|: F)#Point, T#Point[A]]): (G -|: C)#Point[A] = cc.asInstanceOf[(G -|: C)#Point[A]]
      def unpack[A](cc: (G -|: C)#Point[A]): Pivot[(G -|: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G -|: F)#Point, T#Point[A]]]
    }

    implicit def corecursePivot2[G[_[_], _, _], G2[_[_], _, _], Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot, G2[?[_], Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Z, ?] -|: C, Pivot, G2[?[_], Z, ?] -|: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G2[?[_], Z, ?] -|: F)#Point, T#Point[A]]): (G2[?[_], Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Z, ?] -|: C)#Point[A]]
      def unpack[A](cc: (G2[?[_], Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Z, ?] -|: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Z, ?] -|: F)#Point, T#Point[A]]]
    }

    implicit def corecursePivot3[G[_[_], _, _, _], G2[_[_], _, _, _], Y, Z, Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[G, G2], C: NestedAtPoint[C, Pivot, F, T]): NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot, G2[?[_], Y, Z, ?] -|: F, T] = new NestedAtPoint[G2[?[_], Y, Z, ?] -|: C, Pivot, G2[?[_], Y, Z, ?] -|: F, T] {
      def NN = C.NN

      def pack[A](cc: Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, T#Point[A]]): (G2[?[_], Y, Z, ?] -|: C)#Point[A] = cc.asInstanceOf[(G2[?[_], Y, Z, ?] -|: C)#Point[A]]
      def unpack[A](cc: (G2[?[_], Y, Z, ?] -|: C)#Point[A]): Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, T#Point[A]] = cc.asInstanceOf[Pivot[(G2[?[_], Y, Z, ?] -|: F)#Point, T#Point[A]]]
    }
  }
}

object Effects {
  import Properties._

  @implicitNotFound("could not compute a method for mapping over effect stack ${C}; either a member of the stack lacks an Applicative, or its Applicative instance is ambiguous")
  trait Mapper[C <: Effects] {
    type CC[A] = C#Point[A]

    def point[A](a: A): CC[A]

    def map[A, B](fa: CC[A])(f: A => B): CC[B]
  }

  trait MapperLowPriorityImplicits {
    import cats.state.State

    /*implicit def headState[S]: Mapper[State[S, ?] |: Base] = new Mapper[State[S, ?] |: Base] {

      def point[A](a: A) = State.pure(a)

      def map[A, B](fa: State[S, A])(f: A => B): State[S, B] = fa map f
    }

    implicit def corecurseState[S, C <: Effects](implicit P: Mapper[C]): Mapper[State[S, ?] |: C] = new Mapper[State[S, ?] |: C] {

      def point[A](a: A) = State.pure(P.point(a))

      def map[A, B](fa: State[S, C#Point[A]])(f: A => B): State[S, C#Point[B]] = fa map { a => P.map(a)(f) }
    }*/
  }

  object Mapper extends MapperLowPriorityImplicits {

    implicit def base: Mapper[Base] = new Mapper[Base] {
      def point[A](a: A) = a
      def map[A, B](fa: A)(f: A => B) = f(fa)
    }

    implicit def corecurse1[F[_], C <: Effects](implicit P: Mapper[C], NN: NonNested[C], F: Applicative[F]): Mapper[F |: C] = new Mapper[F |: C] {

      def point[A](a: A) = NN.pack(F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack(F.map(NN.unpack(fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[Z, ?]]): Mapper[F2[Z, ?] |: C] = new Mapper[F2[Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[Z, ?], B](F.map(NN.unpack[F2[Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[Y, Z, ?]]): Mapper[F2[Y, Z, ?] |: C] = new Mapper[F2[Y, Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[Y, Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[Y, Z, ?], B](F.map(NN.unpack[F2[Y, Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit P: Mapper[C], NN: NonNested[C], F: Applicative[F[G, ?]]): Mapper[F[G, ?] |: C] = new Mapper[F[G, ?] |: C] {

      def point[A](a: A) = NN.pack[F[G, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F[G, ?], B](F.map(NN.unpack[F[G, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[G, Z, ?]]): Mapper[F2[G, Z, ?] |: C] = new Mapper[F2[G, Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[G, Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[G, Z, ?], B](F.map(NN.unpack[F2[G, Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], P: Mapper[C], NN: NonNested[C], F: Applicative[F2[G, Y, Z, ?]]): Mapper[F2[G, Y, Z, ?] |: C] = new Mapper[F2[G, Y, Z, ?] |: C] {

      def point[A](a: A) = NN.pack[F2[G, Y, Z, ?], A](F.pure(P.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NN.pack[F2[G, Y, Z, ?], B](F.map(NN.unpack[F2[G, Y, Z, ?], A](fa)) { ca => P.map(ca)(f) })
    }

    implicit def pivot1[Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], Pivot: Applicative[Pivot[F#Point, ?]], T: Mapper[T]): Mapper[C] = new Mapper[C] {

      def point[A](a: A): CC[A] = NAP.pack(Pivot.pure(T.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NAP.pack(Pivot.map(NAP.unpack(fa)) { ta => T.map(ta)(f) })
    }

    implicit def pivot2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[Pivot, Pivot2], NAP: NestedAtPoint[C, Pivot2[?[_], Z, ?], F, T], Pivot: Applicative[Pivot2[F#Point, Z, ?]], T: Mapper[T]): Mapper[C] = new Mapper[C] {

      def point[A](a: A): CC[A] = NAP.pack(Pivot.pure(T.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NAP.pack(Pivot.map(NAP.unpack(fa)) { ta => T.map(ta)(f) })
    }

    implicit def pivot3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[Pivot, Pivot2], NAP: NestedAtPoint[C, Pivot2[?[_], Y, Z, ?], F, T], Pivot: Applicative[Pivot2[F#Point, Y, Z, ?]], T: Mapper[T]): Mapper[C] = new Mapper[C] {

      def point[A](a: A): CC[A] = NAP.pack(Pivot.pure(T.point(a)))

      def map[A, B](fa: CC[A])(f: A => B): CC[B] =
        NAP.pack(Pivot.map(NAP.unpack(fa)) { ta => T.map(ta)(f) })
    }
  }

  trait Traverser[C <: Effects] {
    type CC[A] = C#Point[A]

    def traverse[G[_]: Applicative, A, B](ca: CC[A])(f: A => G[B]): G[CC[B]]
    def foldLeft[A, B](fa: CC[A], b: B)(f: (B, A) => B): B
    def foldRight[A, B](fa: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
  }

  object Traverser {

    implicit def base: Traverser[Base] = new Traverser[Base] {
      def traverse[G[_]: Applicative, A, B](fa: A)(f: A => G[B]): G[B] = f(fa)
      def foldLeft[A, B](fa: A, b: B)(f: (B, A) => B) = f(b, fa)
      def foldRight[A, B](fa: A, b: Eval[B])(f: (A, Eval[B]) => Eval[B]) = f(fa, b)
    }

    implicit def corecurse1[F[_], C <: Effects](implicit C: Traverser[C], NN: NonNested[C], F: Traverse[F]): Traverser[F |: C] = new Traverser[F |: C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = F.traverse(NN.unpack(fca)) { ca =>
          C.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NN.pack(fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        F.foldLeft(NN.unpack(fca), b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(NN.unpack(fca), lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }

    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[Z, ?]]): Traverser[F2[Z, ?] |: C] = new Traverser[F2[Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = F.traverse(NN.unpack[F2[Z, ?], A](fca)) { ca =>
          C.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NN.pack[F2[Z, ?], B](fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        F.foldLeft(NN.unpack[F2[Z, ?], A](fca), b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(NN.unpack[F2[Z, ?], A](fca), lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[Y, Z, ?]]): Traverser[F2[Y, Z, ?] |: C] = new Traverser[F2[Y, Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = F.traverse(NN.unpack[F2[Y, Z, ?], A](fca)) { ca =>
          C.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NN.pack[F2[Y, Z, ?], B](fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        F.foldLeft(NN.unpack[F2[Y, Z, ?], A](fca), b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(NN.unpack[F2[Y, Z, ?], A](fca), lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurseH1[F[_[_], _], G0[_], C <: Effects](implicit C: Traverser[C], NN: NonNested[C], F: Traverse[F[G0, ?]]): Traverser[F[G0, ?] |: C] = new Traverser[F[G0, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = F.traverse(NN.unpack[F[G0, ?], A](fca)) { ca =>
          C.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NN.pack[F[G0, ?], B](fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        F.foldLeft(NN.unpack[F[G0, ?], A](fca), b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(NN.unpack[F[G0, ?], A](fca), lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G0[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[G0, Z, ?]]): Traverser[F2[G0, Z, ?] |: C] = new Traverser[F2[G0, Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = F.traverse(NN.unpack[F2[G0, Z, ?], A](fca)) { ca =>
          C.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NN.pack[F2[G0, Z, ?], B](fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        F.foldLeft(NN.unpack[F2[G0, Z, ?], A](fca), b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(NN.unpack[F2[G0, Z, ?], A](fca), lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G0[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Traverser[C], NN: NonNested[C], F: Traverse[F2[G0, Y, Z, ?]]): Traverser[F2[G0, Y, Z, ?] |: C] = new Traverser[F2[G0, Y, Z, ?] |: C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = F.traverse(NN.unpack[F2[G0, Y, Z, ?], A](fca)) { ca =>
          C.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NN.pack[F2[G0, Y, Z, ?], B](fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        F.foldLeft(NN.unpack[F2[G0, Y, Z, ?], A](fca), b) { (b, ca) =>
          C.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        F.foldRight(NN.unpack[F2[G0, Y, Z, ?], A](fca), lb) { (ca, eb) =>
          C.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def pivot1[Pivot[_[_], _], C <: Effects, F <: Effects, T <: Effects](implicit NAP: NestedAtPoint[C, Pivot, F, T], T: Traverser[T], Pivot: Traverse[Pivot[F#Point, ?]]): Traverser[C] = new Traverser[C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = Pivot.traverse(NAP.unpack(fca)) { ca =>
          T.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NAP.pack(fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        Pivot.foldLeft(NAP.unpack(fca), b) { (b, ca) =>
          T.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        Pivot.foldRight(NAP.unpack(fca), lb) { (ca, eb) =>
          T.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def pivot2[Pivot[_[_], _, _], Pivot2[_[_], _, _], Z, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH2[Pivot, Pivot2], NAP: NestedAtPoint[C, Pivot2[?[_], Z, ?], F, T], T: Traverser[T], Pivot: Traverse[Pivot2[F#Point, Z, ?]]): Traverser[C] = new Traverser[C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = Pivot.traverse(NAP.unpack(fca)) { ca =>
          T.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NAP.pack(fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        Pivot.foldLeft(NAP.unpack(fca), b) { (b, ca) =>
          T.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        Pivot.foldRight(NAP.unpack(fca), lb) { (ca, eb) =>
          T.foldRight(ca, eb)(f)
        }
      }
    }

    implicit def pivot3[Pivot[_[_], _, _, _], Pivot2[_[_], _, _, _], Y, Z, C <: Effects, F <: Effects, T <: Effects](implicit ev: PermuteH3[Pivot, Pivot2], NAP: NestedAtPoint[C, Pivot2[?[_], Y, Z, ?], F, T], T: Traverser[T], Pivot: Traverse[Pivot2[F#Point, Y, Z, ?]]): Traverser[C] = new Traverser[C] {

      def traverse[G[_]: Applicative, A, B](fca: CC[A])(f: A => G[B]): G[CC[B]] = {
        val back = Pivot.traverse(NAP.unpack(fca)) { ca =>
          T.traverse(ca)(f)
        }

        Applicative[G].map(back) { fca2 => NAP.pack(fca2) }
      }

      def foldLeft[A, B](fca: CC[A], b: B)(f: (B, A) => B): B = {
        Pivot.foldLeft(NAP.unpack(fca), b) { (b, ca) =>
          T.foldLeft(ca, b)(f)
        }
      }

      def foldRight[A, B](fca: CC[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        Pivot.foldRight(NAP.unpack(fca), lb) { (ca, eb) =>
          T.foldRight(ca, eb)(f)
        }
      }
    }
  }

  @implicitNotFound("could not prove ${C} is a valid monadic stack; perhaps an effect is lacking a FlatMap, or a non-outer effect is lacking a Traverse")
  trait Binder[C <: Effects] {
    type CC[A] = C#Point[A]

    def bind[A, B](cca: CC[A])(f: A => CC[B]): CC[B]
  }

  trait BinderLowPriorityImplicits {
    import cats.state.State

    /*implicit def headState[S]: Binder[State[S, ?] |: Base] = new Binder[State[S, ?] |: Base] {
      def bind[A, B](fa: State[S, A])(f: A => State[S, B]) = fa flatMap f
    }

    implicit def corecurseState[S, C <: Effects](implicit C: Binder[C], T: Traverser[C]): Binder[State[S, ?] |: C] = new Binder[State[S, ?] |: C] {

      def bind[A, B](fa: State[S, C#Point[A]])(f: A => State[S, C#Point[B]]): State[S, C#Point[B]] = {
        fa flatMap { ca =>
          val scca = T.traverse[State[S, ?], A, C#Point[B]](ca) { a => f(a) }

          scca map { cca => C.bind(cca) { a => a } }
        }
      }
    }*/
  }

  object Binder extends BinderLowPriorityImplicits {

    implicit def base: Binder[Base] = new Binder[Base] {
      def bind[A, B](fa: A)(f: A => B): B = f(fa)
    }

    implicit def corecurse1[F[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F], B: FlatMap[F]): Binder[F |: C] = new Binder[F |: C] {

      def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
        val back = B.flatMap(NN.unpack(fca)) { ca =>
          val fcaca = T.traverse(ca) { a => NN.unpack(f(a)) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }

        NN.pack(back)
      }
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Binder[C], NN: NonNested[C], T: Traverser[C], F: Applicative[F2[Z, ?]], B: FlatMap[F2[Z, ?]]): Binder[F2[Z, ?] |: C] = new Binder[F2[Z, ?] |: C] {

      def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
        val back = B.flatMap(NN.unpack[F2[Z, ?], A](fca)) { ca =>
          val fcaca = T.traverse[F2[Z, ?], A, C#Point[B]](ca) { a => NN.unpack[F2[Z, ?], B](f(a)) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }

        NN.pack[F2[Z, ?], B](back)
      }
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Binder[C], NN: NonNested[C], T: Traverser[C], F: Applicative[F2[Y, Z, ?]], B: FlatMap[F2[Y, Z, ?]]): Binder[F2[Y, Z, ?] |: C] = new Binder[F2[Y, Z, ?] |: C] {

      def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
        val back = B.flatMap(NN.unpack[F2[Y, Z, ?], A](fca)) { ca =>
          val fcaca = T.traverse[F2[Y, Z, ?], A, C#Point[B]](ca) { a => NN.unpack[F2[Y, Z, ?], B](f(a)) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }

        NN.pack[F2[Y, Z, ?], B](back)
      }
    }

    implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F[G, ?]], B: FlatMap[F[G, ?]]): Binder[F[G, ?] |: C] = new Binder[F[G, ?] |: C] {

      def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
        val back = B.flatMap(NN.unpack[F[G, ?], A](fca)) { ca =>
          val fcaca = T.traverse[F[G, ?], A, C#Point[B]](ca) { a => NN.unpack[F[G, ?], B](f(a)) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }

        NN.pack[F[G, ?], B](back)
      }
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F2[G, Z, ?]], B: FlatMap[F2[G, Z, ?]]): Binder[F2[G, Z, ?] |: C] = new Binder[F2[G, Z, ?] |: C] {

      def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
        val back = B.flatMap(NN.unpack[F2[G, Z, ?], A](fca)) { ca =>
          val fcaca = T.traverse[F2[G, Z, ?], A, C#Point[B]](ca) { a => NN.unpack[F2[G, Z, ?], B](f(a)) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }

        NN.pack[F2[G, Z, ?], B](back)
      }
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Binder[C], T: Traverser[C], NN: NonNested[C], F: Applicative[F2[G, Y, Z, ?]], B: FlatMap[F2[G, Y, Z, ?]]): Binder[F2[G, Y, Z, ?] |: C] = new Binder[F2[G, Y, Z, ?] |: C] {

      def bind[A, B](fca: CC[A])(f: A => CC[B]): CC[B] = {
        val back = B.flatMap(NN.unpack[F2[G, Y, Z, ?], A](fca)) { ca =>
          val fcaca = T.traverse[F2[G, Y, Z, ?], A, C#Point[B]](ca) { a => NN.unpack[F2[G, Y, Z, ?], B](f(a)) }

          F.map(fcaca) { caca => C.bind(caca) { a => a } }
        }

        NN.pack[F2[G, Y, Z, ?], B](back)
      }
    }
  }

  trait Expander[C <: Effects] {
    type CC[_]
    type Out <: Effects

    type Point[A] = C#Point[A]

    def apply[A](fa: Point[A]): Out#Point[CC[A]]
  }

  trait ExpanderLowPriorityImplicits {
    import cats.state.State

    /*implicit def headState[S]: Expander.Aux[State[S, ?] |: Base, State[S, ?], Base] = new Expander[State[S, ?] |: Base] {
      type CC[A] = State[S, A]
      type Out = Base

      def apply[A](fa: State[S, A]): State[S, A] = fa
    }

    implicit def corecurseState[S, C <: Effects](implicit C: Expander[C]): Expander.Aux[State[S, ?] |: C, C.CC, State[S, ?] |: C.Out] = new Expander[State[S, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = State[S, ?] |: C.Out

      def apply[A](gca: State[S, C#Point[A]]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }*/
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

      def apply[A](gca: Point[A]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Expander[C]): Expander.Aux[F2[Z, ?] |: C, C.CC, F2[Z, ?] |: C.Out] = new Expander[F2[Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[Z, ?] |: C.Out

      def apply[A](gca: Point[A]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Expander[C]): Expander.Aux[F2[Y, Z, ?] |: C, C.CC, F2[Y, Z, ?] |: C.Out] = new Expander[F2[Y, Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[Y, Z, ?] |: C.Out

      def apply[A](gca: Point[A]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH1[F[_[_], _], G[_], C <: Effects](implicit C: Expander[C]): Expander.Aux[F[G, ?] |: C, C.CC, F[G, ?] |: C.Out] = new Expander[F[G, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F[G, ?] |: C.Out

      def apply[A](gca: Point[A]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, C <: Effects](implicit ev: PermuteH2[F, F2], C: Expander[C]): Expander.Aux[F2[G, Z, ?] |: C, C.CC, F2[G, Z, ?] |: C.Out] = new Expander[F2[G, Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[G, Z, ?] |: C.Out

      def apply[A](gca: Point[A]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Expander[C]): Expander.Aux[F2[G, Y, Z, ?] |: C, C.CC, F2[G, Y, Z, ?] |: C.Out] = new Expander[F2[G, Y, Z, ?] |: C] {
      type CC[A] = C.CC[A]
      type Out = F2[G, Y, Z, ?] |: C.Out

      def apply[A](gca: Point[A]): Out#Point[CC[A]] =
        gca.asInstanceOf[Out#Point[CC[A]]]     // already proven equivalent; evaluation requires a Functor
    }
  }

  trait Collapser[E, C <: Effects] {
    type A
    type Out <: Effects

    type Point[A] = C#Point[A]

    def apply(fa: Point[E]): Out#Point[A]
  }

  trait CollapserLowPriorityImplicits1 {
    import cats.state.State

    /*implicit def headState[S, A0]: Collapser.Aux[State[S, A0], Base, A0, State[S, ?] |: Base] = new Collapser[State[S, A0], Base] {
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
    }*/
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

      def apply(gca: Point[E]): Out#Point[A] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH3[E, F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, C <: Effects](implicit ev: PermuteH3[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[G, Y, Z, ?] |: C, C.A, F2[G, Y, Z, ?] |: C.Out] = new Collapser[E, F2[G, Y, Z, ?] |: C] {
      type A = C.A
      type Out = F2[G, Y, Z, ?] |: C.Out

      def apply(gca: Point[E]): Out#Point[A] =
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

      def apply(gca: Point[E]): Out#Point[A] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse2[E, F[_, _], F2[_, _], Z, C <: Effects](implicit ev: Permute2[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[Z, ?] |: C, C.A, F2[Z, ?] |: C.Out] = new Collapser[E, F2[Z, ?] |: C] {
      type A = C.A
      type Out = F2[Z, ?] |: C.Out

      def apply(gca: Point[E]): Out#Point[A] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurse3[E, F[_, _, _], F2[_, _, _], Y, Z, C <: Effects](implicit ev: Permute3[F, F2], C: Collapser[E, C]): Collapser.Aux[E, F2[Y, Z, ?] |: C, C.A, F2[Y, Z, ?] |: C.Out] = new Collapser[E, F2[Y, Z, ?] |: C] {
      type A = C.A
      type Out = F2[Y, Z, ?] |: C.Out

      def apply(gca: Point[E]): Out#Point[A] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }

    implicit def corecurseH1[E, F[_[_], _], G[_], C <: Effects](implicit C: Collapser[E, C]): Collapser.Aux[E, F[G, ?] |: C, C.A, F[G, ?] |: C.Out] = new Collapser[E, F[G, ?] |: C] {
      type A = C.A
      type Out = F[G, ?] |: C.Out

      def apply(gca: Point[E]): Out#Point[A] =
        gca.asInstanceOf[Out#Point[A]]      // already proven equivalent; evaluation requires a Functor
    }
  }

  @implicitNotFound("could not lift ${E} into stack ${C}; either ${C} does not contain a constructor of ${E}, or there is no Functor for a constructor of ${E}")
  trait Lifter[E, C <: Effects] {
    type Out
    type CC[A] = C#Point[A]

    def apply(e: E): CC[Out]
  }

  trait LifterLowPriorityImplicits {
    import cats.state.State

    /*
    implicit def headState[S, A]: Lifter.Aux[State[S, A], State[S, ?] |: Base, A] = new Lifter[State[S, A], State[S, ?] |: Base] {
      type Out = A

      def apply(fa: State[S, A]) = fa
    }

    implicit def midState[S, A, C <: Effects](implicit C: Mapper[C]): Lifter.Aux[State[S, A], State[S, ?] |: C, A] = new Lifter[State[S, A], State[S, ?] |: C] {
      type Out = A

      def apply(fa: State[S, A]) = fa map { a => C.point(a) }
    }

    implicit def corecurseState[S, E, C <: Effects](implicit L: Lifter[E, C]): Lifter.Aux[E, State[S, ?] |: C, L.Out] = new Lifter[E, State[S, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = State.pure(L(e))
    }
    */
  }

  object Lifter extends LifterLowPriorityImplicits {
    type Aux[E, C <: Effects, Out0] = Lifter[E, C] { type Out = Out0 }

    //implicit def base[A]: Lifter[A, Base] = new Lifter[A, Base] {
    //  type Out = A

    //  def apply(fa: A) = fa
    //}

    implicit def mid1[F[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F], NN: NonNested[C]): Lifter.Aux[F[A], F |: C, A] = new Lifter[F[A], F |: C] {
      type Out = A

      def apply(fa: F[A]) = NN.pack(F.map(fa) { a => C.point(a) })
    }

    implicit def mid2[F[_, _], F2[_, _], Z, A, C <: Effects](implicit ev: Permute2[F, F2], C: Mapper[C], F: Functor[F2[Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[Z, A], F2[Z, ?] |: C, A] = new Lifter[F2[Z, A], F2[Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[Z, A]) = NN.pack[F2[Z, ?], A](F.map(fa) { a => C.point(a) })
    }

    implicit def mid3[F[_, _, _], F2[_, _, _], Y, Z, A, C <: Effects](implicit ev: Permute3[F, F2], C: Mapper[C], F: Functor[F2[Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[Y, Z, A], F2[Y, Z, ?] |: C, A] = new Lifter[F2[Y, Z, A], F2[Y, Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[Y, Z, A]) = NN.pack[F2[Y, Z, ?], A](F.map(fa) { a => C.point(a) })
    }

    implicit def midH1[F[_[_], _], G[_], A, C <: Effects](implicit C: Mapper[C], F: Functor[F[G, ?]], NN: NonNested[C]): Lifter.Aux[F[G, A], F[G, ?] |: C, A] = new Lifter[F[G, A], F[G, ?] |: C] {
      type Out = A

      def apply(fa: F[G, A]) = NN.pack[F[G, ?], A](F.map((fa)) { a => C.point(a) })
    }

    implicit def midH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, A, C <: Effects](implicit ev: PermuteH2[F, F2], C: Mapper[C], F: Functor[F2[G, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[G, Z, A], F2[G, Z, ?] |: C, A] = new Lifter[F2[G, Z, A], F2[G, Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[G, Z, A]) = NN.pack[F2[G, Z, ?], A](F.map(fa) { a => C.point(a) })
    }

    implicit def midH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, A, C <: Effects](implicit ev: PermuteH3[F, F2], C: Mapper[C], F: Functor[F2[G, Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: C, A] = new Lifter[F2[G, Y, Z, A], F2[G, Y, Z, ?] |: C] {
      type Out = A

      def apply(fa: F2[G, Y, Z, A]) = NN.pack[F2[G, Y, Z, ?], A](F.map(fa) { a => C.point(a) })
    }

    implicit def corecurse1[F[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F], NN: NonNested[C]): Lifter.Aux[E, F |: C, L.Out] = new Lifter[E, F |: C] {
      type Out = L.Out

      def apply(e: E) = NN.pack(F.pure(L(e)))
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects](implicit ev: Permute2[F, F2], L: Lifter[E, C], F: Applicative[F2[Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[Z, ?] |: C, L.Out] = new Lifter[E, F2[Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = NN.pack[F2[Z, ?], Out](F.pure(L(e)))
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, E, C <: Effects](implicit ev: Permute3[F, F2], L: Lifter[E, C], F: Applicative[F2[Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[Y, Z, ?] |: C, L.Out] = new Lifter[E, F2[Y, Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = NN.pack[F2[Y, Z, ?], Out](F.pure(L(e)))
    }

    implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects](implicit L: Lifter[E, C], F: Applicative[F[G, ?]], NN: NonNested[C]): Lifter.Aux[E, F[G, ?] |: C, L.Out] = new Lifter[E, F[G, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = NN.pack[F[G, ?], Out](F.pure(L(e)))
    }

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, E, C <: Effects](implicit ev: PermuteH2[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[G, Z, ?] |: C, L.Out] = new Lifter[E, F2[G, Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = NN.pack[F2[G, Z, ?], Out](F.pure(L(e)))
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, E, C <: Effects](implicit ev: PermuteH3[F, F2], L: Lifter[E, C], F: Applicative[F2[G, Y, Z, ?]], NN: NonNested[C]): Lifter.Aux[E, F2[G, Y, Z, ?] |: C, L.Out] = new Lifter[E, F2[G, Y, Z, ?] |: C] {
      type Out = L.Out

      def apply(e: E) = NN.pack[F2[G, Y, Z, ?], Out](F.pure(L(e)))
    }
  }

  @implicitNotFound("could not infer effect stack ${C} from type ${E}")
  trait Wrapper[E, C <: Effects] {
    type A
    type CC[A] = C#Point[A]

    def apply(e: E): CC[A]
  }

  trait WrapperLowPriorityImplicits1 {
    import cats.state.State

    implicit def head[A0]: Wrapper.Aux[A0, Base, A0] = new Wrapper[A0, Base] {
      type A = A0

      def apply(a: A) = a
    }

    // state's definition in scalaz is weird enough to confuse scalac, but it's an important effect to support
    /*implicit def corecurseState[S, E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[State[S, E], State[S, ?] |: C, A0] = new Wrapper[State[S, E], State[S, ?] |: C] {
      type A = A0

      def apply(s: State[S, E]): State[S, C#Point[A0]] = s map { e => W(e) }
    }*/
  }

  // not really sure why these functions in particular need to be moved down
  trait WrapperLowPriorityImplicits2 extends WrapperLowPriorityImplicits1 {

    implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, E, C <: Effects, A0](implicit ev: PermuteH2[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[G, Z, E], F2[G, Z, ?] |: C, A0] = new Wrapper[F2[G, Z, E], F2[G, Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[G, Z, E]): CC[A] =
        fe.asInstanceOf[CC[A]]      // already proven equivalent; actual evaluation requires a Functor
    }

    implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, E, C <: Effects, A0](implicit ev: PermuteH3[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[G, Y, Z, E], F2[G, Y, Z, ?] |: C, A0] = new Wrapper[F2[G, Y, Z, E], F2[G, Y, Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[G, Y, Z, E]): CC[A] =
        fe.asInstanceOf[CC[A]]      // already proven equivalent; actual evaluation requires a Functor
    }
  }

  object Wrapper extends WrapperLowPriorityImplicits2 {
    type Aux[E, C <: Effects, A0] = Wrapper[E, C] { type A = A0 }

    implicit def corecurse1[F[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[E], F |: C, A0] = new Wrapper[F[E], F |: C] {
      type A = A0

      def apply(fe: F[E]): CC[A] =
        fe.asInstanceOf[CC[A]]      // already proven equivalent; actual evaluation requires a Functor
    }

    implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects, A0](implicit ev: Permute2[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[Z, E], F2[Z, ?] |: C, A0] = new Wrapper[F2[Z, E], F2[Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[Z, E]): CC[A] =
        fe.asInstanceOf[CC[A]]
    }

    implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, E, C <: Effects, A0](implicit ev: Permute3[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[Y, Z, E], F2[Y, Z, ?] |: C, A0] = new Wrapper[F2[Y, Z, E], F2[Y, Z, ?] |: C] {
      type A = A0

      def apply(fe: F2[Y, Z, E]): CC[A] =
        fe.asInstanceOf[CC[A]]
    }

    implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[G, E], F[G, ?] |: C, A0] = new Wrapper[F[G, E], F[G, ?] |: C] {
      type A = A0

      def apply(fe: F[G, E]): CC[A] =
        fe.asInstanceOf[CC[A]]      // already proven equivalent; actual evaluation requires a Functor
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

  implicit def functorInstance[C <: Effects](implicit C: Mapper[C]): Functor[Emm[C, ?]] = new Functor[Emm[C, ?]] {

    def map[A, B](fa: Emm[C, A])(f: A => B): Emm[C, B] = new Emm(C.map(fa.run)(f))
  }
}

trait EmmLowPriorityImplicits2 extends EmmLowPriorityImplicits1 {
  import Effects._

  implicit def monadInstance[C <: Effects : Mapper : Binder]: Monad[Emm[C, ?]] = new Monad[Emm[C, ?]] {

    def pure[A](a: A): Emm[C, A] = new Emm(implicitly[Mapper[C]].point(a))

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
}
