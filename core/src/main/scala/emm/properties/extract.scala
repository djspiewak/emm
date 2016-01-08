package emm
package properties

sealed trait BarExtract[T <: Effects, C <: Effects] {
  type F[_]
}

object BarExtract {
  type Aux[T <: Effects, C <: Effects, F0[_]] = BarExtract[T, C] { type F[A] = F0[A] }

  implicit def extract1[F0[_], T <: Effects]: BarExtract.Aux[T, F0 |: T, F0] = new BarExtract[T, F0 |: T] {
    type F[A] = F0[A]
  }

  implicit def extract2[F0[_, _], F02[_, _], Z, T <: Effects](implicit ev: Permute2[F0, F02]): BarExtract.Aux[T, F02[Z, ?] |: T, F02[Z, ?]] = new BarExtract[T, F02[Z, ?] |: T] {
    type F[A] = F02[Z, A]
  }

  implicit def extract3[F0[_, _, _], F02[_, _, _], Y, Z, T <: Effects](implicit ev: Permute3[F0, F02]): BarExtract.Aux[T, F02[Y, Z, ?] |: T, F02[Y, Z, ?]] = new BarExtract[T, F02[Y, Z, ?] |: T] {
    type F[A] = F02[Y, Z, A]
  }

  implicit def extractH1[F0[_[_], _], G[_], T <: Effects]: BarExtract.Aux[T, F0[G, ?] |: T, F0[G, ?]] = new BarExtract[T, F0[G, ?] |: T] {
    type F[A] = F0[G, A]
  }

  implicit def extractH2[F0[_[_], _, _], F02[_[_], _, _], G[_], Z, T <: Effects](implicit ev: PermuteH2[F0, F02]): BarExtract.Aux[T, F02[G, Z, ?] |: T, F02[G, Z, ?]] = new BarExtract[T, F02[G, Z, ?] |: T] {
    type F[A] = F02[G, Z, A]
  }

  implicit def extractH3[F0[_[_], _, _, _], F02[_[_], _, _, _], G[_], Y, Z, T <: Effects](implicit ev: PermuteH3[F0, F02]): BarExtract.Aux[T, F02[G, Y, Z, ?] |: T, F02[G, Y, Z, ?]] = new BarExtract[T, F02[G, Y, Z, ?] |: T] {
    type F[A] = F02[G, Y, Z, A]
  }
}

sealed trait PivotExtract[T <: Effects, C <: Effects] {
  type Pivot[_[_], _]
}

object PivotExtract {
  type Aux[T <: Effects, C <: Effects, Pivot0[_[_], _]] = PivotExtract[T, C] { type Pivot[F[_], A] = Pivot0[F, A] }

  implicit def extract1[Pivot0[_[_], _], T <: Effects]: PivotExtract.Aux[T, Pivot0 -|: T, Pivot0] = new PivotExtract[T, Pivot0 -|: T] {
    type Pivot[F[_], A] = Pivot0[F, A]
  }

  implicit def extract2[Pivot0[_[_], _, _], Z, T <: Effects]: PivotExtract.Aux[T, Pivot0[?[_], Z, ?] -|: T, Pivot0[?[_], Z, ?]] = new PivotExtract[T, Pivot0[?[_], Z, ?] -|: T] {
    type Pivot[F[_], A] = Pivot0[F, Z, A]
  }

  implicit def extract3[Pivot0[_[_], _, _, _], Y, Z, T <: Effects]: PivotExtract.Aux[T, Pivot0[?[_], Y, Z, ?] -|: T, Pivot0[?[_], Y, Z, ?]] = new PivotExtract[T, Pivot0[?[_], Y, Z, ?] -|: T] {
    type Pivot[F[_], A] = Pivot0[F, Y, Z, A]
  }
}