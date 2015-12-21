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
def readName: Task[String] = ???
def log(msg: String): Task[Unit] = ???

val effect: OptionT[Task, String] = for {
  first <- readName.liftM[OptionT]
  last <- readName.liftM[OptionT]

  name <- (if ((first.length * last.length) < 20) OptionT.some[Task, String](s"$first $last") else OptionT.none[Task, String])

  _ <- log(s"successfully read in $name").liftM[OptionT]
} yield name
```

The advantages of `Emm` become much more apparent when attempting to stack more than just two monads simultaneously.  For example, one might imagine stacking `Task`, `Option` and right-biased `Either`.  Let's enrich our previous example with some error handling (using kind projector to avoid a type lambda):

```scala
def readName: Task[String] = ???
def log(msg: String): Task[Unit] = ???

type E = Task |: Either[String, _] |: Option |: Base

val effect: Emm[E, String] = for {
  first <- readName.liftM[E]
  last <- readName.liftM[E]

  name <- (if ((first.length * last.length) < 20) Some(s"$first $last") else None).liftM[E]

  _ <- (if (name == "Daniel Spiewak") Left("your kind isn't welcome here") else Right(())).liftM[E]

  _ <- log(s"successfully read in $name").liftM[E]
} yield name
```

Simple and straightforward, with all the same syntax as before.  However, if we look at the same example using monad transformers, a rather distopian picture emerges:

```scala
def readName: Task[String] = ???
def log(msg: String): Task[Unit] = ???

val effect: OptionT[EitherT[Task, String, _], String] = for {
  first <- readName.liftM[EitherT[Task, String, _]].liftM[OptionT]
  last <- readName.liftM[EitherT[Task, String, _]].liftM[OptionT]

  name <- (if ((first.length * last.length) < 20) OptionT.some[EitherT[Task, String, _], String](s"$first $last") else OptionT.none[EitherT[Task, String, _], String])

  _ <- (if (name == "Daniel Spiewak") EitherT.left[Task, String, Unit]("your kind isn't welcome here") else EitherT.right[Task, String, Unit](())).liftM[OptionT]

  _ <- log(s"successfully read in $name").liftM[EitherT[Task, String, _]].liftM[OptionT]
} yield name
```

That's a *lot* of very explicit lifting and special syntax.  I had to ponder quite long and hard about the above, and I'm not even sure if I got it all right!  Monad transformers are very ugly, very cumbersome, and when you get things wrong they explode in remarkably spectacular ways.

The `Emm` monad is intended to change all of that.  It is intended to be very straightforward to manage and extend complex stacks of effects, and to do so without any special wrappers or added complexity from the effect author.  No need to write an `OptionT`, just use `Option`!

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