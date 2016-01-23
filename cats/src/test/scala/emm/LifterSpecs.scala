package emm

import emm.compat.cats._

import org.specs2.mutable._

import cats._
import cats.data._
import cats.std.list._
import cats.std.option._
import cats.free.Free

import scalaz.concurrent.Task

object LifterSpecs extends Specification with TestHelpers {

  "simple effect composition with lifter" should {

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

    "lift into a stack that contains a kleisli" in {
      import cats.data.Kleisli

      "inner" >> {
        type E = Option |: Kleisli[?[_], String, ?] -|: Base

        Kleisli.pure[λ[X => X], String, Int](12).liftM[E].run.run("foo") must beSome(12)
        Option(42).liftM[E].run.run("foo") must beSome(42)
      }

      "outer" >> {
        type E = Free[List, ?] |: Option |: Base

        Free.pure[List, Int](42).liftM[E].run.runM(identity) mustEqual List(Option(42))
        Option(42).liftM[E].run.runM(identity) mustEqual List(Option(42))
      }
    }
  }
}
