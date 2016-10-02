package examples.services

import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Abstracting over the return type
  * Abstracting over implementations
  * TODO: Add cake
  *
  *
  * https://t.co/CWQyyhgTb8
  *
  * Cats https://www.scala-exercises.org/cats/semigroup
  *
  * runMain examples.services.Runner
  *
  */
class μservice[F[_] : Effect : cats.Functor] {
  import cats.data.Xor
  import cats.implicits._

  val fetchUser: Service[F, Long, TimeoutException Xor User] =
    (id: Long) =>
      examples.services.Effect[F].from(scala.util.Try(fetchRemoteUser(id))) //F[scala.util.Try[User]]
        .xor(TimeoutException(s"User $id fetch timeout"))

  val fetchAddress: Service[F, Long, TimeoutException Xor Address] =
    (id: Long) =>
      examples.services.Effect[F].from(scala.util.Try(fetchRemoteAddress(id))) //F[scala.util.Try[Address]]
        .xor(TimeoutException(s"Address $id fetch timeout"))

  val fetchOptionUser: Service[F, Long, NotFound Xor User] =
    (id: Long) =>
      examples.services.Effect[F].from(Option(fetchRemoteUser(id))) //F[Option[User]]
        .xor(NotFound(s"User $id not found"))

  val fetchOptionAddress: Service[F, Long, NotFound Xor Address] =
    (id: Long) =>
      examples.services.Effect[F].from(Option(fetchRemoteAddress(id))) //F[Option[Address]]
        .xor(NotFound(s"Address $id not found"))

  private def fetchRemoteUser(id: Long): User = {
    //throw new Exception("Error user fetch")
    //null
    println(Thread.currentThread.getName)
    User(userId = id, addressId = 101l)
  }

  private def fetchRemoteAddress(addressId: Long): Address = {
    //throw new Exception("Error address fetch")
    //null
    println(Thread.currentThread.getName)
    Address(addressId)
  }
}

object μservice {

  def apply[F[_] : Effect : cats.Monad : cats.RecursiveTailRecM]: μservice[F] = new μservice[F]
}

object Runner extends App {
  import cats._
  implicit val ec = ExecutionContext.Implicits.global

  import cats.instances.try_._
  import cats.instances.future._
  import monix.cats._

  //val a = MicroServices[Future].fetchUser(101l)
  //val b = MicroServices[monix.eval.Task].fetchUser(101l)
  //val c = MicroServices[scala.util.Try].fetchUser(111l)


  //Abstracting over the return type
  for {
    a <- μservice[Future].fetchUser(101l)
    b <- μservice[Future].fetchAddress(24l)
    //a <- μservice[Future].fetchOptionAddress(101l)
    //b <- μservice[Future].fetchOptionAddress(24l)
  } yield println(a, b)


  //Abstracting over implementations
  import algebra._
  implicit val M = μservice[Future]
  val futureResult = fetchBoth(201l).foldMap(interpreter[Future])
  import scala.concurrent.duration._
  println(Await.result(futureResult, 3 second))
}