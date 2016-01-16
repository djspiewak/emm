package emm
package effects

import shims.{Applicative, FlatMap, Functor, Monad, Traverse}
import scala.annotation.implicitNotFound

import properties._

@implicitNotFound("could not infer effect stack ${C} from type ${E}")
trait Wrapper[E, C <: Effects] {
  type A
  type CC[A] = C#Point[A]

  def apply(e: E): CC[A]
}

trait WrapperLowPriorityImplicits1 {

  implicit def head[A0]: Wrapper.Aux[A0, Base, A0] = new Wrapper[A0, Base] {
    type A = A0

    def apply(a: A) = a
  }
}

// not really sure why these functions in particular need to be moved down
trait WrapperLowPriorityImplicits2 extends WrapperLowPriorityImplicits1 {

  def corecurse1[F[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[E], F |: C, A0]

  implicit def corecurseH2[F[_[_], _, _], F2[_[_], _, _], G[_], Z, E, C <: Effects, A0](implicit ev: PermuteH2[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[G, Z, E], F2[G, Z, ?] |: C, A0] = corecurse1[F2[G, Z, ?], E, C, A0]
  implicit def corecurseH3[F[_[_], _, _, _], F2[_[_], _, _, _], G[_], Y, Z, E, C <: Effects, A0](implicit ev: PermuteH3[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[G, Y, Z, E], F2[G, Y, Z, ?] |: C, A0] = corecurse1[F2[G, Y, Z, ?], E, C, A0]
}

object Wrapper extends WrapperLowPriorityImplicits2 {
  type Aux[E, C <: Effects, A0] = Wrapper[E, C] { type A = A0 }

  implicit def corecurse1[F[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[E], F |: C, A0] = new Wrapper[F[E], F |: C] {
    type A = A0

    def apply(fe: F[E]): CC[A] =
      fe.asInstanceOf[CC[A]]      // already proven equivalent; actual evaluation requires a Functor
  }

  implicit def corecurse2[F[_, _], F2[_, _], Z, E, C <: Effects, A0](implicit ev: Permute2[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[Z, E], F2[Z, ?] |: C, A0] = corecurse1[F2[Z, ?], E, C, A0]
  implicit def corecurse3[F[_, _, _], F2[_, _, _], Y, Z, E, C <: Effects, A0](implicit ev: Permute3[F, F2], W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F2[Y, Z, E], F2[Y, Z, ?] |: C, A0] = corecurse1[F2[Y, Z, ?], E, C, A0]

  implicit def corecurseH1[F[_[_], _], G[_], E, C <: Effects, A0](implicit W: Wrapper.Aux[E, C, A0]): Wrapper.Aux[F[G, E], F[G, ?] |: C, A0] = corecurse1[F[G, ?], E, C, A0]
}