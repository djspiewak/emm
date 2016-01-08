package emm
package properties

sealed trait Extract[T <: Effects, C <: Effects] {
  type F[_]
}

object Extract {
  type Aux[T <: Effects, C <: Effects, F0[_]] = Extract[T, C] { type F[A] = F0[A] }

  implicit def extract1[F0[_], T <: Effects]: Extract.Aux[T, F0 |: T, F0] = new Extract[T, F0 |: T] {
    type F[A] = F0[A]
  }

  implicit def extract2[F0[_, _], F02[_, _], Z, T <: Effects](implicit ev: Permute2[F0, F02]): Extract.Aux[T, F02[Z, ?] |: T, F02[Z, ?]] = new Extract[T, F02[Z, ?] |: T] {
    type F[A] = F02[Z, A]
  }

  implicit def extract3[F0[_, _, _], F02[_, _, _], Y, Z, T <: Effects](implicit ev: Permute3[F0, F02]): Extract.Aux[T, F02[Y, Z, ?] |: T, F02[Y, Z, ?]] = new Extract[T, F02[Y, Z, ?] |: T] {
    type F[A] = F02[Y, Z, A]
  }

  implicit def extractH1[F0[_[_], _], G[_], T <: Effects]: Extract.Aux[T, F0[G, ?] |: T, F0[G, ?]] = new Extract[T, F0[G, ?] |: T] {
    type F[A] = F0[G, A]
  }

  implicit def extractH2[F0[_[_], _, _], F02[_[_], _, _], G[_], Z, T <: Effects](implicit ev: PermuteH2[F0, F02]): Extract.Aux[T, F02[G, Z, ?] |: T, F02[G, Z, ?]] = new Extract[T, F02[G, Z, ?] |: T] {
    type F[A] = F02[G, Z, A]
  }

  implicit def extractH3[F0[_[_], _, _, _], F02[_[_], _, _, _], G[_], Y, Z, T <: Effects](implicit ev: PermuteH3[F0, F02]): Extract.Aux[T, F02[G, Y, Z, ?] |: T, F02[G, Y, Z, ?]] = new Extract[T, F02[G, Y, Z, ?] |: T] {
    type F[A] = F02[G, Y, Z, A]
  }
}