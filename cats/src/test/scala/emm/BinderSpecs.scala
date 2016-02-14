package emm

import emm.compat.cats._

import org.specs2.mutable._

import cats._
import cats.data._
import cats.free.Free
import cats.std.list._
import cats.std.option._
import cats.std.function._

import scalaz.concurrent.Task

object BinderSpecs extends Specification with TestHelpers {

  "simple effect composition with binder" should {

    "allow binding" in {
      type E = List |: Option |: Base

      val e = for {
        v <- List(1, 2, 3, 4).liftM[E]
        v2 <- (Some(v) filter { _ % 2 == 0 }).liftM[E]
      } yield v2

      e mustEqual Emm[E, Int](List(None, Some(2), None, Some(4)))
    }

    "allow binding over a Option of Kleisli of List" in {
      type E = Option |: Kleisli[?[_], Int, ?] -|: List |: Base

      import effects._
      import properties._

      "foobar".pointM[E].flatMap(x => (x + "baz").pointM[E]).run.run(42) mustEqual Some(List("foobarbaz"))
    }

    "allow binding over a Option of List of Kleisli" in {
      type E = Option |: List |: Kleisli[?[_], Int, ?] -|: Base

      "foobar".pointM[E].flatMap(x => (x + "baz").pointM[E]).run.run(42) mustEqual Some(List("foobarbaz"))
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

    "bind over a stack that contains state" in {
      "inner" >> {
        type E = Option |: StateT[?[_], String, ?] -|: Base

         (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.runA("blah") must beSome("foo")
       }

       "mid" >> {
         type E = List |: StateT[?[_], String, ?] -|: Option |: Base

         (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.runA("blah") mustEqual List(Some("foo"))
      }
    }

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

  "non-traversable effect composition with binder" should {

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
  }
}
