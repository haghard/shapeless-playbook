package examples

import monix.eval.Task
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

package object services {
  import cats.{Functor, Foldable}
  import cats.implicits._
  import cats.syntax.either._
  import cats.instances.either._


  type Service[F[_], A, B] = A ⇒ F[B]

  sealed abstract class AppException(msg: String) extends Product with Serializable

  final case class NotFound(msg: String) extends AppException(msg)

  final case class DuplicateFound(msg: String) extends AppException(msg)

  final case class TimeoutException(msg: String) extends AppException(msg)

  final case class HostNotFoundException(msg: String) extends AppException(msg)

  case class Address(addressId: Long)
  case class User(userId: Long, addressId: Long)

  @simulacrum.typeclass trait Effect[F[_]] {
    def from[A](a: ⇒ A): F[A]
  }

  implicit def futureEffect(implicit ec: ExecutionContext): Effect[Future] =
    new Effect[Future] {
      override def from[A](a: => A): Future[A] = Future(a)(ec)
    }

  implicit val taskEffect: Effect[Task] =
    new Effect[Task] {
      override def from[A](a: => A): Task[A] = Task.evalOnce(a)
    }

  implicit val tryEffect: Effect[Try] =
    new Effect[Try] {
      override def from[A](a: => A): Try[A] = Try(a)
    }

  implicit class FGOps[F[_] : Functor, G[_] : Foldable, A](fa: F[G[A]]) {
    def xor[L](ex: L): F[L Either A] =
      Functor[F].map(fa) { g =>
        Foldable[G].foldLeft[A, L Either A](g, { println("Or" + ex); Left[L,A](ex)}) { (_, b) =>
          println(b)
          Right[L,A](b)
        }
      }
  }
}