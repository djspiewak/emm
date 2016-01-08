package emm
package properties

import cats.{Applicative, FlatMap, Functor, Monad, Traverse, Eval}
import scala.annotation.implicitNotFound

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

  implicit def corecurse[C <: Effects, C2 <: Effects](implicit E: BarExtract[C, C2], C: NonNested[C]): NonNested[C2] = new NonNested[C2] {
    def pack[CC[_], A](cc: CC[C2#Point[A]]) = cc.asInstanceOf[C2#Build[CC, A]]
    def unpack[CC[_], A](cc: C2#Build[CC, A]) = cc.asInstanceOf[CC[C2#Point[A]]]
  }
}