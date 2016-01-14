package emm

import emm.cats._

import org.specs2.mutable._

import _root_.cats._
import _root_.cats.data._
import _root_.cats.std.list._
import _root_.cats.std.option._

import scalaz.concurrent.Task

object MapperSpecs extends Specification with TestHelpers {

  "simple effect composition with mapper" should {

    "allow mapping" in {
      val opt: Option[Int] = Some(42)
      val e = opt.liftM[List |: Option |: Base]

      e map (2 *) mustEqual Emm[List |: Option |: Base, Int](List(Some(84)))
    }

    "allow mapping over a Kleisli" in {
      type E = Option |: Kleisli[?[_], Int, ?] -|: Base

      "foobar".pointM[E].map(_ + "baz").run.run(42) must beSome("foobarbaz")
    }

    "allow mapping over a List of Option of Kleisli" in {
      type E = List |: Option |: Kleisli[?[_], Int, ?] -|: Base

      "foobar".pointM[E].map(_ + "baz").run.run(42) mustEqual List(Some("foobarbaz"))
    }

    "allow mapping over a Option of Kleisli of List" in {
      type E = Option |: Kleisli[?[_], Int, ?] -|: List |: Base

      "foobar".pointM[E].map(_ + "baz").run.run(42) mustEqual Some(List("foobarbaz"))
    }
  }

  "non-traversable effect composition with mapper" should {

    "allow mapping in either direction" in {
      val opt: Option[Int] = Some(42)

      opt.liftM[Task |: Option |: Base] map (2 *)
      opt.liftM[Option |: Task |: Base] map (2 *)

      ok
    }
  }
}
