package emm
package effects

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}
import scala.annotation.implicitNotFound

import properties._

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