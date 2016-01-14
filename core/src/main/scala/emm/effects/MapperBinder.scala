package emm
package effects

import shims.Monad

private[emm] object MapperBinder {

  implicit def monad[F <: Effects](implicit FB: Binder[F], FM: Mapper[F]): Monad[F#Point] = new Monad[F#Point] {
    def point[A](a: A): F#Point[A] = FM.point(a)
    override def map[A, B](fa: F#Point[A])(f: A => B): F#Point[B] = FM.map(fa)(f)
    def flatMap[A, B](fa: F#Point[A])(f: A => F#Point[B]): F#Point[B] = FB.bind(fa)(f)
  }
}