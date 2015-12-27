package emm

sealed trait Permute2[F[_, _], G[_, _]]

object Permute2 {
  implicit def identity[F[_, _]]: Permute2[F, F] = new Permute2[F, F] {}
  implicit def flip[F[_, _]]: Permute2[F, λ[(A, B) => F[B, A]]] = new Permute2[F, λ[(A, B) => F[B, A]]] {}
}

sealed trait Permute3[F[_, _, _], G[_, _, _]]

object Permute3 {
  implicit def abc[F[_, _, _]]: Permute3[F, F] = new Permute3[F, F] {}
  implicit def acb[F[_, _, _]]: Permute3[F, λ[(A, B, C) => F[A, C, B]]] = new Permute3[F, λ[(A, B, C) => F[A, C, B]]] {}
  implicit def bac[F[_, _, _]]: Permute3[F, λ[(A, B, C) => F[B, A, C]]] = new Permute3[F, λ[(A, B, C) => F[B, A, C]]] {}
  implicit def bca[F[_, _, _]]: Permute3[F, λ[(A, B, C) => F[B, C, A]]] = new Permute3[F, λ[(A, B, C) => F[B, C, A]]] {}
  implicit def cab[F[_, _, _]]: Permute3[F, λ[(A, B, C) => F[C, A, B]]] = new Permute3[F, λ[(A, B, C) => F[C, A, B]]] {}
  implicit def cba[F[_, _, _]]: Permute3[F, λ[(A, B, C) => F[C, B, A]]] = new Permute3[F, λ[(A, B, C) => F[C, B, A]]] {}
}

// for reasons of sanity, we're only going to support left-biased higher-order type constructors (for now)
sealed trait PermuteH2[F[_[_], _, _], G[_[_], _, _]]

object PermuteH2 {
  implicit def identity[F[_[_], _, _]]: PermuteH2[F, F] = new PermuteH2[F, F] {}
  implicit def flip[F[_[_], _, _]]: PermuteH2[F, λ[(G[_], A, B) => F[G, B, A]]] = new PermuteH2[F, λ[(G[_], A, B) => F[G, B, A]]] {}
}

// this case gets us scalaz's State, which is all I really care about
sealed trait PermuteH3[F[_[_], _, _, _], G[_[_], _, _, _]]

object PermuteH3 {
  implicit def abc[F[_[_], _, _, _]]: PermuteH3[F, F] = new PermuteH3[F, F] {}
  implicit def acb[F[_[_], _, _, _]]: PermuteH3[F, λ[(G[_], A, B, C) => F[G, A, C, B]]] = new PermuteH3[F, λ[(G[_], A, B, C) => F[G, A, C, B]]] {}
  implicit def bac[F[_[_], _, _, _]]: PermuteH3[F, λ[(G[_], A, B, C) => F[G, B, A, C]]] = new PermuteH3[F, λ[(G[_], A, B, C) => F[G, B, A, C]]] {}
  implicit def bca[F[_[_], _, _, _]]: PermuteH3[F, λ[(G[_], A, B, C) => F[G, B, C, A]]] = new PermuteH3[F, λ[(G[_], A, B, C) => F[G, B, C, A]]] {}
  implicit def cab[F[_[_], _, _, _]]: PermuteH3[F, λ[(G[_], A, B, C) => F[G, C, A, B]]] = new PermuteH3[F, λ[(G[_], A, B, C) => F[G, C, A, B]]] {}
  implicit def cba[F[_[_], _, _, _]]: PermuteH3[F, λ[(G[_], A, B, C) => F[G, C, B, A]]] = new PermuteH3[F, λ[(G[_], A, B, C) => F[G, C, B, A]]] {}
}
