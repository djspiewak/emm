package emm

import emm.cats._

import org.specs2.mutable._

import _root_.cats.std.list._
import _root_.cats.std.option._
import _root_.cats.std.future._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object FutureSpecs extends Specification {
  implicit val ec = scala.concurrent.ExecutionContext.global

  "emm with future" should {
    "compose with list and option" in {
      type E = Future |: Option |: List |: Base

      val bam = for {
        f <- Future(3).liftM[E]
        o <- Option(2).liftM[E]
        l <- List(1, 1).liftM[E]
      } yield f + o + l

      Await.result(bam.run, 20 seconds) must beSome(List(6, 6))
    }
  }
}
