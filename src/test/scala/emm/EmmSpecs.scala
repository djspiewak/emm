package emm

import org.specs2.mutable._

import cats._
import cats.data._
import cats.state.State
import cats.free.Free
import cats.std.list._
import cats.std.option._
import cats.std.function._

import scalaz.concurrent.Task
import scalaz.\/-

object EmmSpecs extends Specification with TestHelpers {

  "simple effect composition" should {
    "define pointM" in {
      42.pointM[Option |: List |: Base] mustEqual Emm[Option |: List |: Base, Int](Option(List(42)))
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
  }

  "non-traversable effect composition" should {
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
}
