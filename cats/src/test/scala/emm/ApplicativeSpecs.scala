package emm

import org.specs2.mutable._

import cats._
import cats.std.list._
import cats.std.option._

/*object ApplicativeSpecs extends Specification {
  "derived applicative" should {
    "be consistent with bind in Option |: List" in {
      type E = Option |: List |: Base

      val a = Emm[E, Int](None)
      val b = Emm[E, Int => Int](Some(Nil))

      // swap the following two blocks and the test will fail

      // val A = Emm.applicativeInstance[E]
      // val M = Emm.monadInstance[E]

      val A = Applicative[Emm[E, ?]]
      val M = Monad[Emm[E, ?]]

      A.ap(a)(b) mustEqual (M.flatMap(b) { b => M.map(a)(b) })
      M.ap(a)(b) mustEqual (M.flatMap(b) { b => M.map(a)(b) })
    }
  }
}*/
