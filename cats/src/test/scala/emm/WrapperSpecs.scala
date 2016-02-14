package emm

import emm.compat.cats._

import org.specs2.mutable._

import cats._
import cats.data._
import cats.free.Free
import cats.std.option._
import cats.std.list._

object WrapperSpecs extends Specification with TestHelpers {

  "simple effect composition with wrapper" should {

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
  }
}
