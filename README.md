# Generalized Effect Composition

Otherwise known as "less confusing monad transformers".

The `Emm` monad provides a syntactically lightweight, type-inference friendly data type for composing effects.  The general motivation is very similar to monad transformers, but the end result is far more user friendly and also significantly more general.  The main goals of the project are as follows:

- Simple and easy to understand
- Clean type inference
- Clean type *errors* (dear god, monad transformer compile errors...)
- Compatibility with pre-existing monads

These goals are very similar to those which motivated [Oleg's `Eff`](http://okmij.org/ftp/Haskell/extensible/), which is a really terrific data structure.  There are some significant differences though.  Most notably, `Eff` requires effect implementations to be rewritten to be compatible with its internal calculus, and so it does not allow the composition of arbitrary "standalone" monads written in a conventional style.  However, `Eff` is able to provide much greater expressive power than `Emm` (or monad transformers) in several key diminsions.  Oleg goes into significant detail on the expressiveness gains of `Eff` in his paper describing the construct.  `Emm` does not provide the same benefits.

## Example

```scala
import emm._
import scalaz.concurrent.Task
import scalaz.std.option._

def readName: Task[String] = ???
def log(msg: String): Task[Unit] = ???

type E = Task |: Option |: Base

val effect: Emm[E, String] = for {
  first <- readName.liftM[E]
  last <- readName.liftM[E]

  name <- (if ((first.length * last.length) < 20) Some(s"$first $last") else None).liftM[E]

  _ <- log(s"successfully read in $name").liftM[E]
} yield name
```

The above is analogous to monad transformers in many ways.  In fact, we can write the exact same code from above using `OptionT`:

```scala
import scalaz._
import scalaz.concurrent.Task
import scalaz.syntax.monad._

def readName: Task[String] = ???
def log(msg: String): Task[Unit] = ???

val effect: OptionT[Task, String] = for {
  first <- readName.liftM[OptionT]
  last <- readName.liftM[OptionT]

  name <- (if ((first.length * last.length) < 20) OptionT.some[Task, String](s"$first $last") else OptionT.none[Task, String])

  _ <- log(s"successfully read in $name").liftM[OptionT]
} yield name
```

The advantages of `Emm` become much more apparent when attempting to stack more than just two monads simultaneously.  For example, one might imagine stacking `Task`, `Option` and right-biased `Either`.  Let's enrich our previous example with some error handling (note that I'm using [kind projector](https://github.com/non/kind-projector) to avoid type lambdas):

```scala
import emm._

import scalaz._
import scalaz.concurrent.Task
import scalaz.std.option._

def readName: Task[String] = ???
def log(msg: String): Task[Unit] = ???

type E = Task |: (String \/ ?) |: Option |: Base

val effect: Emm[E, String] = for {
  first <- readName.liftM[E]
  last <- readName.liftM[E]

  name <- (if ((first.length * last.length) < 20) Some(s"$first $last") else None).liftM[E]

  _ <- (if (name == "Daniel Spiewak") -\/("your kind isn't welcome here") else \/-(())).liftM[E]

  _ <- log(s"successfully read in $name").liftM[E]
} yield name
```

It works as expected, with all the same syntax as before. However, if we look at the same example using monad transformers, a rather distopian picture emerges:

```scala
import scalaz._
import scalaz.concurrent.Task
import scalaz.syntax.monad._

def readName: Task[String] = ???
def log(msg: String): Task[Unit] = ???

val effect: OptionT[EitherT[Task, String, ?], String] = for {
  first <- readName.liftM[EitherT[?[_], String, ?]].liftM[OptionT]
  last <- readName.liftM[(EitherT[?[_], String, ?]].liftM[OptionT]

  name <- if ((first.length * last.length) < 20)
    OptionT.some[EitherT[Task, String, ?], String](s"$first $last")
  else
    OptionT.none[EitherT[Task, String, ?], String]

  _ <- (if (name == "Daniel Spiewak")
    EitherT.fromDisjunction[Task](\/.left[String, Unit]("your kind isn't welcome here"))
  else
    EitherT.fromDisjunction[Task](\/.right[String, Unit](()))).liftM[OptionT]

  _ <- log(s"successfully read in $name").liftM[EitherT[?[_], String, ?]].liftM[OptionT]
} yield name
```

That's a *lot* of very explicit lifting and special syntax.  I had to ponder quite long and hard about the above, and I'm not even sure if I got it all right!  Monad transformers are very ugly, very cumbersome, and when you get things wrong they explode in remarkably spectacular ways.

The `Emm` monad is intended to change all of that.  It is intended to be very straightforward to manage and extend complex stacks of effects, and to do so without any special wrappers or added complexity from the effect author.  No need to write an `OptionT`, just use `Option`!

## API

The following API is provided.  For starters, the following pair of functions are implicitly provided to lift values into the effect stack:

- `pointM[C <: Effects]` – Points a value of type `A` into the monad, `Emm[C, A]`.  Requires an `Applicative` for each component of the effect stack `C`.
- `liftM[C <: Effects]` – Given an effect which is of a type contained within `C`, lift the effect into the full effect stack represented by `C`.  For example: `Option(42).liftM[Task |: Option |: Base]`
- `wrapM[C <: Effects]` – Given a full stack of effects which matches the stack `C`, wrap the stack in the `Emm` monad.  Note that the `C` parameter can be inferred basically 100% of the time, but can be provided explicitly to assert correctness.  Example: `(Task now Option(42)).wrapM`.  This is equivalent to calling the `Emm(...)` constructor, but the type inference is much nicer.

These methods are exposed via implicit classes contained within the `emm` package object.  The `Emm` monad itself provides the following (effective) API:

- `map[B](A => B): Emm[C, B]` – Conventional functor map.  Transforms the value within the effect
- `flatMap[B](A => Emm[C, B]): Emm[C, B]` – Monadic bind.  Transforms the value within the effect and joins the two effect stacks.  This function requires that all components of `C` define a `bind` function, and all components aside from the outer-most (left-most) must have a `Traverse` instance.
- `flatMapM[G[_], B](A => G[B]): Emm[C, B]` – Similar to `flatMap`, except instead of transforming the value to an effect contained within the entire effect stack, `C`, it transforms the value to a single component of that effect stack.  Thus, `G` must be in `C`.  The result is joined with the effect stack and returned within `Emm`.
- `expand` – The inverse of `collapse`.  Converts an `Emm` of the form `Emm[... |: F |: Base, A]` into `Emm[... |: Base, F[A]]`.  This is extremely useful when there are effect-specific functions (e.g. `Option#getOrElse`) that you need to access on the inner-most (right-most) effect of the stack.  Once you have expanded, you can use `map` or `flatMap` to access these functions and manipulate the inner-most effect.  Runs in constant time.
- `collapse` – The inverse of `expand`.  Converts an `Emm` of the form `Emm[... |: Base, F[A]]` into `Emm[... |: F |: Base, A]`.  This is generally most useful in conjunction with `expand`, where you have manipulated the inner-most effect and you need to "recombine" the results of that manipulation with the full effect stack.  Runs in constant time.
- `run` – Unwraps the effect stack (without modification) from `Emm`.  Effectively, this takes a type of the form `Emm[F |: G |: Base, A]` and produces a type of the form `F[G[A]]`.  Literally, it is the "contents" of `Emm`.

## Requirements

Right now, this is sitting on top of the scalaz 7.1 typeclass hierarchy, but it could be adapted to cats (or any other hierarchy) almost trivially.  Everything is implemented in terms of the following type classes (with minimal constraints for every function):

- `Applicative`
- `Bind`
- `Functor`
- `Traverse`

### Invalid and Partially-Valid Stacks

Constraints which are not required to evaluate a given function are not assumed.  For example, consider the following effect stack:

```scala
type E = Option |: Task |: Base

val effect = Option(42).liftM[E]
```

If you attempt to run `flatMap` on this effect stack, you will run into problems:

```scala
effect flatMapM { i => if (i < 20) None else Some(i * 2) }          // does not compile!
```

This will fail because `Task` is *not* the outer-most effect, which is to say, it isn't the effect on the far left of the effect definition.  The reason this is a problem becomes more clear if we look at things in terms of `map`, `flatten` and the raw stack, rather than simply `flatMap` and the collapsed `Emm` monad:

```scala
val effect2: Option[Task[Int]] = Some(Task now 42)

val mapped: Option[Task[Option[Task[Int]]]] = effect2 map { t =>
  t map { i =>
    if (i < 20) None else Some(Task now (i * 2))
  }
}

val result: Option[Task[Int]] = mapped.flatten    // ??????
```

Notice the problem here.  We need to take the second `Option` layer, which is *within* a `Task`, and "flip" it outside of the `Task` layer in order to flatten the `Option` and `Task` layers together.  Basically, we want to do something like this:

```scala
Option[Task[Option[Task[Int]]]] => Option[Option[Task[Task[Int]]]] => Option[Task[Task[Int]]] => Option[Task[Int]]
```

Clearly, there are no problems with the last two stages, but that second stage is completely impossible.  We can't take a value from inside `Task` and "flip" it to the outside.  `Task` is basically a `Future`, so the value "inside" of `Task` doesn't even exist yet!  So this effect stack is non-sensical as a monad; we cannot define `flatMap` (or equivalently, `flatMapM`) on it, and the compiler is very happy to tell us so.

Technically, the reason we *can't* do this is because there is no instance `Traverse[Task]`, and in fact you cannot define such an instance without actually running the `Task`.  Our example from earlier though, where our stack was `Task |: Option |: Base` was just fine, because there *is* an instance `Traverse[Option]`.

Here's the cool bit though.  Even though it doesn't make any sense to define `flatMap` on `Emm[Option |: Task |: Base, Int]`, there's no reason why we can't define `map`!

```scala
type E = Option |: Task |: Base

val effect = Option(42).liftM[E]

val effect2 = effect map { _ * 2 }          // no problemo!
```

Even though our effect stack is sort of bogus, it's only bogus if we attempt to treat it *as a monad*.  It's a perfectly valid applicative functor, and we can treat it as such.  In other words, `flatMap` doesn't work (and shouldn't work!) on some effect stacks, but `map` works on any effect stack where each component effect has a `Functor`.

## Limitations

Maybe this section should be nearer to the top...  Oh well.

The most significant limitation of this approach is caused by everyone's favorite limitation of the scalac type checker, [SI-2712](https://issues.scala-lang.org/browse/SI-2712).  The good news is that this bug is not a complete show stopper; it's relatively easy to work around when you control the entire stack of type signatures (as I do here) and you're not trying to generalize over different type constructor arities.  The bad news is that it makes my life very difficult, and it imposes some pretty hard limits (also related to how much boilerplate I'm willing to type out) on what sorts of type constructors do and do not work with `Emm`.

Specifically, the following *kinds* of type constructors are accepted (i.e. will be fully functional in any position of an effect stack):

- `* -> *` – Examples: `Option`, `List`, `Task`
- `* x * -> *` – Examples: `Either`, `State` (with caveats), `Writer` (more caveats), `Reader` (sorry, still caveated)
- `* x * x * -> *` – Examples: *No idea*
- `(* -> *) x * -> *` – Examples: `Free`, `OptionT`, `ListT`, `StreamT`
- `(* -> *) x * x * -> *` – Examples: *uh...*
- `(* -> *) x * x * x * -> *` – Examples: `IndexedStateT` (sort of)
- `State` – Yes, this is actually special-cased

First off, the examples in the higher-order entry (such as `ListT`) are not a typo.  You can (sort of) freely mix monad transformers and `Emm`, though I can only think of one reason why you would want to: effects implemented as transformers on the `Identity` monad.  These implementations are the bane of my existence, but unfortunately they are extremely common due to the nature of scalaz's (and cats') design.

For example, let's imagine that we wanted to compose the `StreamT` effect with `Emm`.  We obviously don't care about the fact that `StreamT` can act as a transformer, since `Emm` handles that for us, so we define a new type alias:

```scala
type Stream[A] = StreamT[Identity, A]
```

Seems reasonable.  Now, it would be *intuitive* to expect that this puts us into the `* -> *` case from above, but surprisingly scalac does not agree!  Basically, scalac's type inference algorithm expands fully fixed type aliases (i.e. non-dependent stuff) *before* it solves any expressions, which means that the following will fail to compile:

```scala
def foo[F[_], A](fa: F[A]) = fa

val xs: Stream[A] = ???

foo(xs)       // boom!
```

It doesn't work, because scalac is looking for something of kind `* -> *` and it finds something of kind `(* -> *) -> * -> *`: namely, `StreamT`.  The fact that what it finds has *already* been partially-applied on `Identity` is irrelevant, since no part of scalac's type inference really understands partial application at the type level.  Most people who run into SI-2712 do so through this avenue.  Basically, scalac is going to drill down *all the way* to the "base type", which is generally-speaking either a dependent type, a class or a trait.  The form of *that* type is what matters.

What does this have to do with us?  Well, it's easier to see if we look at `State`.  Remember, the `State` monad *in theory* has the following signature:

```scala
class State[S, A](f: S => (S, A))
```

Unfortunately, neither scalaz nor cats define their `State` monad in this way.  Scalaz is particularly egregious, since `State` is defined in terms of not one but *two* partial applications:

```scala
type State[S, A] = StateT[Id, S, A]
type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
```

Dear god, what is `IndexedStateT`??  No one really knows, but it's highly inconvenient.  You'll notice that the kind of `IndexedStateT` (the base type) is `(* -> *) x * x * x * -> *`.  Now, this type signature is in our list, which seems like it should be ok, but unfortunately it turns out that there is one more foible: the `Id` type is treated specially by scalac.

I have no idea why this is, and honestly it's not something I had heard about before.  For context, `Id` is defined in the following way:

```scala
type Id[+X] = X
```

Seems straightforward enough.  The variance is scary, because variance is always scary, but generally there's just not much going on here.  The problem is that this type doesn't appear to *consistently* unify with types that have the shape `* -> *`, which is precisely what our `IndexedStateT` pattern seems to require.  In other words, *I can't get `State` to work!*

This is unbelievably frustrating, and `State` is a really really important type of effect.  So it's special-cased.  There is literally a separate case (at a lower priority) in all of the implicit machinery for `State` specifically, and this works.  So, you *can* use `State` with `Emm`, and you basically don't have to worry about any of these shenanigans.  It's only ugly on my end, and then only because scalaz decided to define `State` in terms of a whole series of increasingly-complex monad transformer signatures.

If monad transformers weren't a thing (or more notably, if it weren't idiomatic to define all effect types as transformers), none of this would be an issue.  Unfortunately, `Emm` has to be compatible with this sort of legacy mess, and so `State` is special cased.  Yay?

The good news is that overcoming these limitations is largely a matter of typing, albeit a *lot* of typing.  It's certainly possible to enable support for `State` (and similarly-defined effects), but I haven't done it yet because the work involved is both significant and boring.  In the meantime, only effects with a base type who's kind is on the list above are supported by `Emm`.

Moral of the story: library authors, newtype your effects!  It makes everyone's lives easier.