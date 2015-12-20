package emm

import org.specs2.mutable._

import scalaz.concurrent.Task
import scalaz.std.list._
import scalaz.std.option._

object EmmSpecs extends Specification {

  "simple effect composition" should {
    "allow lifting in either direction" in {
      val opt: Option[Int] = Some(42)

      opt.liftM[Option |: List |: CCNil]
      opt.liftM[List |: Option |: CCNil]

      ok
    }

    "allow mapping" in {
      val opt: Option[Int] = Some(42)
      val e = opt.liftM[List |: Option |: CCNil]

      e map (2 *) mustEqual Emm[List |: Option |: CCNil, Int](List(Some(84)))
    }

    "allow binding" in {
      type E = List |: Option |: CCNil

      val e = for {
        v <- List(1, 2, 3, 4).liftM[E]
        v2 <- (Some(v) filter { _ % 2 == 0 }).liftM[E]
      } yield v2

      e mustEqual Emm[E, Int](List(None, Some(2), None, Some(4)))
    }
  }

  "non-traversable effect composition" should {
    "allow mapping in either direction" in {
      val opt: Option[Int] = Some(42)

      opt.liftM[Task |: Option |: CCNil] map (2 *)
      opt.liftM[Option |: Task |: CCNil] map (2 *)

      ok
    }

    "allow binding where the non-traversable effect is outermost" in {
      type E = Task |: Option |: CCNil
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