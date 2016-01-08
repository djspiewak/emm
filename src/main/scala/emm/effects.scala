package emm

// there's a bug in scalac's cyclic checking with open recursion and type constructors with arity > 1
trait Partial {
  type Apply[F[_]]
}

sealed trait Effects {
  type Point[A] = Build[λ[X => X], A]

  type Build[CC[_], A] = Inner[A]#Apply[CC]

  type Inner[A] <: Partial

  type Append[E <: Effects] <: Effects
}

sealed trait |:[F[_], T <: Effects] extends Effects {
  type Inner[A] = Partial { type Apply[CC[_]] = T#Inner[A]#Apply[λ[X => CC[F[X]]]] }
  type Append[E <: Effects] = F |: T#Append[E]
}

sealed trait -|:[F[_[_], _], T <: Effects] extends Effects {
  type Inner[A] = Partial { type Apply[CC[_]] = F[CC, T#Inner[A]#Apply[λ[X => X]]] }
  type Append[E <: Effects] = F -|: T#Append[E]
}

sealed trait Base extends Effects {
  type Inner[A] = Partial { type Apply[F[_]] = F[A] }
  type Append[E <: Effects] = E
}
