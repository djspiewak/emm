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

object ExpanderCollapserSpecs extends Specification with TestHelpers {

  "basic simple effect composition" should {

    "define pointM" in {
      42.pointM[Option |: List |: Base] mustEqual Emm[Option |: List |: Base, Int](Option(List(42)))
    }
  }

  "non-traversable effect composition with expander and collapser" should {

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
