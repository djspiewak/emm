package emm

import emm.compat.scalaz._

import org.specs2.mutable._

import scalaz._
import scalaz.std.option._
import scalaz.std.list._

object ScalazSpecs extends Specification {

  "state stacks" should {
    "implement bind" in {
      "inner" >> {
        type E = Option |: StateT[?[_], String, ?] -|: Base

         (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.eval("blah") must beSome("foo")
       }

       "mid" >> {
         type E = List |: StateT[?[_], String, ?] -|: Option |: Base

         (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.eval("blah") mustEqual List(Some("foo"))
      }
    }
  }

  "kleisli stacks" should {
    "implement bind" in {
      "inner" >> {
        type E = Option |: Kleisli[?[_], String, ?] -|: Base

         (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.run("blah") must beSome("foo")
       }

       "mid" >> {
         type E = List |: Kleisli[?[_], String, ?] -|: Option |: Base

         (42.pointM[E] flatMap { _ => "foo".pointM[E] }).run.run("blah") mustEqual List(Some("foo"))
      }
    }
  }
}