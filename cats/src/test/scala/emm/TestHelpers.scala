package emm

import org.specs2.matcher._

import _root_.cats._
import _root_.cats.data._
import _root_.cats.free.Free

import scalaz.concurrent.Task

import scala.reflect.runtime.universe.TypeTag


trait TestHelpers {

  def haveType[A](implicit A: TypeTag[A]) = new {

    def attempt[B](implicit B: TypeTag[B]): Matcher[B] = new Matcher[B] {
      def apply[B2 <: B](s: Expectable[B2]) =
        result(A.tpe =:= B.tpe, s"${s.description} has type ${A.tpe}", s"${s.description} does not have type ${A.tpe}; has type ${B.tpe}", s)
    }
  }

  implicit val taskFlatMap: Monad[Task] = new Monad[Task] {
    import scalaz.concurrent.Future

    def pure[A](x: A): Task[A] = new Task(Future.delay(Task.Try(x)))

    def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = {
      fa.flatMap(f)
    }
  }

  implicit def freeTraverse[F[_]](implicit trF: Traverse[F]): Traverse[Free[F, ?]] = new Traverse[Free[F, ?]] {

    def traverse[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit ap: Applicative[G]): G[Free[F, B]] =
      fa.resume match {
        case Xor.Left(s) => ap.map(trF.traverse(s)(fa => traverse[G, A ,B](fa)(f)))(ffa => Free.liftF(ffa).flatMap(a => a))
        case Xor.Right(a) => ap.map(f(a))(Free.pure(_))
      }

    def foldLeft[A, B](fa: Free[F, A], b: B)(f: (B, A) => B): B = ???

    def foldRight[A, B](fa: Free[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }
}
