package emm

sealed trait Permute2[F[_, _], G[_, _]]

object Permute2 {
  implicit def identity[F[_, _]]: Permute2[F, F] = new Permute2[F, F] {}
  implicit def flip[F[_, _]]: Permute2[F, λ[(A, B) => F[B, A]]] = new Permute2[F, λ[(A, B) => F[B, A]]] {}
}
