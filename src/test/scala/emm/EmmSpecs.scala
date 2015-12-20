package emm

import org.specs2.mutable._

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
}