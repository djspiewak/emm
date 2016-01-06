package emm

import org.specs2.mutable._
import org.specs2.matcher._

import cats._
import cats.data._
import cats.state.State
import cats.free.Free
import cats.std.list._
import cats.std.option._
import cats.std.function._

import scalaz.concurrent.Task
import scalaz.\/-

import scala.reflect.runtime.universe.TypeTag

object EmmSpecs extends Specification {

  implicit def freeTraverse[F[_]](implicit trF: Traverse[F]): Traverse[Free[F, ?]] = new Traverse[Free[F, ?]] {
    def traverse[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit ap: Applicative[G]): G[Free[F, B]] =
      fa.resume match {
        case Xor.Left(s) => ap.map(trF.traverse(s)(fa => traverse[G, A ,B](fa)(f)))(ffa => Free.liftF(ffa).flatMap(a => a))
        case Xor.Right(a) => ap.map(f(a))(Free.pure(_))
      }

    def foldLeft[A, B](fa: Free[F, A], b: B)(f: (B, A) => B): B = ???

    def foldRight[A, B](fa: Free[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

  implicit val taskFlatMap: Monad[Task] = new Monad[Task] {
    import scalaz.concurrent.Future
    def pure[A](x: A): Task[A] = new Task(Future.delay(Task.Try(x)))
    
    def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = {
      fa.flatMap(f)
    }
  }

  "simple effect composition" should {
    "define pointM" in {
      42.pointM[Option |: List |: Base] mustEqual Emm[Option |: List |: Base, Int](Option(List(42)))
    }

    "allow lifting in either direction" in {
      val opt: Option[Int] = Some(42)

      opt.liftM[Option |: List |: Base] must haveType[Emm[Option |: List |: Base, Int]].attempt
      opt.liftM[List |: Option |: Base] must haveType[Emm[List |: Option |: Base, Int]].attempt
    }

    "lift into an effect stack of depth three" in {
      type E = Task |: List |: Option |: Base

      Option(42).liftM[E].run.run mustEqual List(Option(42))
      List(42).liftM[E].run.run mustEqual List(Option(42))
      (Task now 42).liftM[E].run.run mustEqual List(Option(42))
    }

    "lift into a stack that contains a partially-applied arity-2 constructor" in {
      "inner" >> {
        type E = Option |: (String Xor ?) |: Base

        Xor.right[String, Int](42).liftM[E] mustEqual Emm[E, Int](Option(Xor.right(42)))
        Xor.left[String, Int]("fuuuuuu").liftM[E] mustEqual Emm[E, Int](Option(Xor.left("fuuuuuu")))
      }

      "outer" >> {
        type E = (String Xor ?) |: Option |: Base

        Xor.right[String, Int](42).liftM[E] mustEqual Emm[E, Int](Xor.right(Option(42)))
        Xor.left[String, Int]("fuuuuuu").liftM[E] mustEqual Emm[E, Int](Xor.left("fuuuuuu"))
      }
    }

    "lift into a stack that contains a partially-applied arity-2 higher-order constructor" in {
      "inner" >> {
        type E = Option |: Free[List, ?] |: Base

        Free.pure[List, Int](42).liftM[E].run must beLike {
          case Some(f) => f runM identity mustEqual List(42)
        }

        Option(42).liftM[E].run must beLike {
          case Some(f) => f runM identity mustEqual List(42)
        }
      }

      "outer" >> {
        type E = Free[List, ?] |: Option |: Base

        Free.pure[List, Int](42).liftM[E].run.runM(identity) mustEqual List(Option(42))
        Option(42).liftM[E].run.runM(identity) mustEqual List(Option(42))
      }
    }

    "lift into a stack that contains a partially-applied arity-2 higher-order constructor and an arity-2 constructor" in {
      "inner" >> {
        type E = (String Xor ?) |: Free[List, ?] |: Base

        Free.pure[List, Int](42).liftM[E].run must beLike {
          case Xor.Right(f) => f runM identity mustEqual List(42)
        }

        Xor.right[String, Int](42).liftM[E].run must beLike {
          case Xor.Right(f) => f runM identity mustEqual List(42)
        }
      }

      "outer" >> {
        type E = Free[List, ?] |: (String Xor ?) |: Base

        Free.pure[List, Int](42).liftM[E].run.runM(identity) mustEqual List(Xor.right(42))
        Xor.right[String, Int](42).liftM[E].run.runM(identity) mustEqual List(Xor.right(42))
      }
    }

    "allow wrapping of two paired constructors" in {
      Option(List(42)).wrapM must haveType[Emm[Option |: List |: Base, Int]].attempt
      Option(List(42)).wrapM[Option |: List |: Base] must haveType[Emm[Option |: List |: Base, Int]].attempt
    }

    "allow wrapping of two paired constructors where one has arity-2" in {
      "inner" >> {
        type E = Option |: (String Xor ?) |: Base

        Option(Xor.right[String, Int](42)).wrapM must haveType[Emm[Option |: (String Xor ?) |: Base, Int]].attempt
        Option(Xor.right[String, Int](42)).wrapM[E] must haveType[Emm[Option |: (String Xor ?) |: Base, Int]].attempt
      }

      "outer" >> {
        type E = (String Xor ?) |: Option |: Base

        Xor.right[String, Option[Int]](Option(42)).wrapM must haveType[Emm[(String Xor ?) |: Option |: Base, Int]].attempt
        Xor.right[String, Option[Int]](Option(42)).wrapM[E] must haveType[Emm[(String Xor ?) |: Option |: Base, Int]].attempt
      }
    }

    "allow wrapping of two paired constructors where one is higher-order and has arity-2" in {
      "inner" >> {
        type E = Option |: Free[List, ?] |: Base

        Option(Free.pure[List, Int](42)).wrapM must haveType[Emm[Option |: Free[List, ?] |: Base, Int]].attempt
        Option(Free.pure[List, Int](42)).wrapM[E] must haveType[Emm[Option |: Free[List, ?] |: Base, Int]].attempt
      }

      "outer" >> {
        type E = Free[List, ?] |: Option |: Base

        Free.pure[List, Option[Int]](Option(42)).wrapM must haveType[Emm[Free[List, ?] |: Option |: Base, Int]].attempt
        Free.pure[List, Option[Int]](Option(42)).wrapM[E] must haveType[Emm[Free[List, ?] |: Option |: Base, Int]].attempt
      }
    }

    //"allow wrapping of two paired constructors where one is state" in {
    //  "inner" >> {
    //    type E = Option |: State[String, ?] |: Base

    //    import Effects._

    //    import cats.state.StateT
    //    import cats.free.Trampoline

    //    val foo = implicitly[Wrapper[State[String, Int], State[String, ?] |: Base]](Wrapper.corecurseH2[StateT, StateT, Trampoline, String, State[String, Int], Base, Int])

    //    Option(State.pure[String, Int](42)).wrapM must haveType[Emm[Option |: State[String, ?] |: Base, Int]].attempt
    //    Option(State.pure[String, Int](42)).wrapM[E](foo) must haveType[Emm[Option |: State[String, ?] |: Base, Int]].attempt
    //  }

    //  "outer" >> {
    //    type E = State[String, ?] |: Option |: Base


    //    import Effects._
    //    val foo = implicitly[Wrapper[State[String, Option[Int]], E]]

    //    State.pure[String, Option[Int]](Option(42)).wrapM must haveType[Emm[State[String, ?] |: Option |: Base, Int]].attempt
    //    State.pure[String, Option[Int]](Option(42)).wrapM[E](foo) must haveType[Emm[State[String, ?] |: Option |: Base, Int]].attempt
    //  }
    //}

    "allow mapping" in {
      val opt: Option[Int] = Some(42)
      val e = opt.liftM[List |: Option |: Base]

      e map (2 *) mustEqual Emm[List |: Option |: Base, Int](List(Some(84)))
    }

    "allow binding" in {
      type E = List |: Option |: Base

      val e = for {
        v <- List(1, 2, 3, 4).liftM[E]
        v2 <- (Some(v) filter { _ % 2 == 0 }).liftM[E]
      } yield v2

      e mustEqual Emm[E, Int](List(None, Some(2), None, Some(4)))
    }

    "bind over a stack that contains a partially-applied arity-2 constructor" in {
      type E = (String Xor ?) |: Base

      42.pointM[E] flatMap { _ => "foo".pointM[E] } mustEqual Emm[E, String](Xor.right("foo"))
    }

    "bind over a stack that contains a partially-applied arity-2 higher-order constructor" in {
      "base" >> {
        type E = Free[List, ?] |: Base

        (42.pointM[E] flatMap { _ => "foo".pointM[E] } run).runM(identity) mustEqual List("foo")
      }

      "inner" >> {
        type E = Option |: Free[List, ?] |: Base

        (42.pointM[E] flatMap { _ => "foo".pointM[E] } run) must beLike {
          case Some(f) => f.runM(identity) mustEqual List("foo")
        }
      }

      "outer" >> {
        type E = Free[List, ?] |: Option |: Base

        (42.pointM[E] flatMap { _ => "foo".pointM[E] } run).runM(identity) mustEqual List(Option("foo"))
      }
    }

    //"bind over a stack that contains state" in {
    //  "empty" >> {
    //    type E = State[String, ?] |: Base

    //    (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.runA("blah").run mustEqual "foo"
    //  }

    //  "outer" >> {
    //    type E = State[String, ?] |: Option |: Base

    //    (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.runA("blah").run must beSome("foo")
    //  }
    //}

    "enable flatMapM in any direction" in {
      type E = List |: Option |: Base

      val e1 = List(1, 2, 3, 4).liftM[E]
      val e2 = e1 flatMapM { v => Some(v) filter { _ % 2 == 0 } }
      val e3 = e2 flatMapM { v => List(v, v) }

      e3 mustEqual Emm[E, Int](List(None, Some(2), Some(2), None, Some(4), Some(4)))
    }

    "allow flatMapM on a stack containing an arity-2 constructor" in {
      type E = List |: (String Xor ?) |: Base

      val e1 = List(1, 2, 3, 4).liftM[E]
      val e2 = e1 flatMapM { v => if (v % 2 == 0) Xor.right(v) else Xor.left("that's... odd") }
      val e3 = e2 flatMapM { v => List(v, v) }

      e3 mustEqual Emm[E, Int](List(Xor.left("that's... odd"), Xor.right(2), Xor.right(2), Xor.left("that's... odd"), Xor.right(4), Xor.right(4)))
    }

    "allow flatMapM on a stack containing a higher-order arity-2 constructor" in {
      type E = List |: Free[Option, ?] |: Base

      val e1 = List(1, 2, 3, 4).liftM[E]
      val e2 = e1 flatMapM { v => if (v % 2 == 0) Free.pure[Option, Int](v) else Free.liftF[Option, Int](None) }
      val e3 = e2 flatMapM { v => List(v, v) }

      e3.run must beLike {
        case List(f1, f2, f3, f4, f5, f6) => {
          f1.runM(identity) mustEqual None
          f2.runM(identity) mustEqual Some(2)
          f3.runM(identity) mustEqual Some(2)
          f4.runM(identity) mustEqual None
          f5.runM(identity) mustEqual Some(4)
          f6.runM(identity) mustEqual Some(4)
        }
      }
    }
  }

  "non-traversable effect composition" should {
    "allow mapping in either direction" in {
      val opt: Option[Int] = Some(42)

      opt.liftM[Task |: Option |: Base] map (2 *)
      opt.liftM[Option |: Task |: Base] map (2 *)

      ok
    }

    "allow binding where the non-traversable effect is outermost" in {
      type E = Task |: Option |: Base
      val opt: Option[Int] = Some(42)

      var sink = 0

      val e = for {
        i <- opt.liftM[E]
        _ <- (Task delay { sink += i }).liftM[E]
      } yield ()

      e.run.run

      sink mustEqual 42
    }

    "enable access to the base of the stack" in {
      type E = Task |: Option |: Base
      val opt: Option[Int] = None

      // this type infers better than a single function
      val e = opt.liftM[E].expand map { _ getOrElse 12 }

      e.run.run mustEqual 12
    }

    "allow both expansion and collapse of base" in {
      type E = Task |: Option |: Base
      val opt: Option[Int] = None

      // this type infers better than a single function
      val e = opt.liftM[E].expand map { _ orElse Some(24) } collapse

      e.run.run must beSome(24)
    }

    "allow both expansion and collapse of base with an arity-2 constructor" in {
      "inner" >> {
        type E = Task |: (String Xor ?) |: Base

        val e = (Task now 42).liftM[E].expand map { _.swap } collapse

        e.run.run mustEqual Xor.left(42)
      }

      "outer" >> {
        type E = (String Xor ?) |: Task |: Base

        val e = (Task now 42).liftM[E].expand map { _.attempt } collapse

        e.run must beLike {
          case Xor.Right(t) => t.run mustEqual \/-(42)
        }
      }
    }

    //"allow both expansion and collapse of base with state" in {
    //  "inner" >> {
    //    type E = Task |: State[String, ?] |: Base

    //    val e = (Task now 42).liftM[E].expand map { s => State.pure[String, Int](s.runA("blerg").run + 12) } collapse

    //    e.run.run.runA("boo").run mustEqual 54
    //  }

    //  "outer" >> {
    //    type E = State[String, ?] |: Task |: Base

    //    val e = (Task now 42).liftM[E].expand map { _.attempt } collapse

    //    e.run.runA("boo").run.run mustEqual \/-(42)
    //  }
    //}

    "allow both expansion and collapse of base with a higher-order arity-2 constructor" in {
      val toList = new (Option ~> List) {
        def apply[A](xs: Option[A]) = xs.toList
      }

      "inner" >> {
        type E = Task |: Free[Option, ?] |: Base

        val e = (Task now 42).liftM[E].expand map { _ mapSuspension toList } collapse

        e.run.run.runM(identity) mustEqual List(42)
      }

      "outer" >> {
        type E = Free[Option, ?] |: Task |: Base

        val e = (Task now 42).liftM[E].expand map { _.attempt } collapse

        e.run.runM(identity) must beLike {
          case Some(t) => t.run mustEqual \/-(42)
        }
      }
    }
  }

  def haveType[A](implicit A: TypeTag[A]) = new {
    def attempt[B](implicit B: TypeTag[B]): Matcher[B] = new Matcher[B] {
      def apply[B2 <: B](s: Expectable[B2]) =
        result(A.tpe =:= B.tpe, s"${s.description} has type ${A.tpe}", s"${s.description} does not have type ${A.tpe}; has type ${B.tpe}", s)
    }
  }
}
