package examples

import monix.eval.Task

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

package object services {

  import cats.{Foldable, Functor}

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

  /*
  Functor[Future].map(Future(Success(0))) { g =>
    Foldable[Try].foldLeft[Int, String Either Int](g, Left("error")) { (_, b) => Right(b) }
  }

  def call: Future[Try[Int]] = Future(Failure(new Exception("some error")))
    //Future(Success(0))

  Functor[Future].map(call) { g =>
    Foldable[Try].foldLeft[Int, String Either Int](g, Left("error")) { (error, b) =>
      println(error)
      Right(b)
    }
  }*/

  /*Functor[Future].map(call) { g =>
    cats.data.Validated.fromTry(g).toEither
  }*/

  //Traverse[Try].traverse(g) { r => Right(r) }

  //Foldable[Try].fold(g)(Monoid[Int])

  //Foldable[Try].fo.foldLeft[Int, String Either Int](g, Left("error")) { (_, b) => Right(b) }


  //example Future[Try[User]]
  //or      Task[Try[User]]
  implicit class FGOps[F[_] : Functor, G[_] : Foldable, A](fa: F[G[A]]) {
    def xor[L](ex: L): F[L Either A] =
      Functor[F].map(fa) { g =>
        Foldable[G].foldLeft[A, L Either A](g, Left[L, A](ex)) { (_, b) => Right[L, A](b) }
      }
  }

}