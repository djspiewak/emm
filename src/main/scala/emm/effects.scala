package emm

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