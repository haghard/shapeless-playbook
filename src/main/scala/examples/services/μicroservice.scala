package examples.services
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * https://www.scala-exercises.org/cats/semigroup
  *
  * runMain examples.services.Runner
  *
  */
class μicroservice[F[_] : Effect : cats.Functor] {
  import cats.data.Xor
  import cats.implicits._

  val fetchUser: Service[F, UserId, TimeoutException Xor User] = {
    (userId: UserId) =>
      val r: F[Try[User]] = examples.services.Effect[F].from(Try(fetchRemoteUser(userId)))
      FGOps[F, Try, User](r).toXor(TimeoutException(s"User $userId not found"))
  }

  val fetchOptionalUser: Service[F, UserId, NotFound Xor User] = {
    (userId: UserId) =>
      val r: F[Option[User]] = examples.services.Effect[F].from(Option(fetchRemoteUser(userId)))
      FGOps[F, Option, User](r).toXor(NotFound(s"User $userId not found"))
  }

  val fetchAddress: Service[F, AddressId, NotFound Xor Address] = {
    (addressId: AddressId) =>
      val r: F[Try[Address]] = examples.services.Effect[F].from(Try(fetchRemoteAddress(addressId)))
      FGOps[F, Try, Address](r).toXor(NotFound(s"Address $addressId not found"))
  }

  def fetchRemoteUser(userId0: UserId): User =
    //throw new Exception("error user lookup")
    //null
    User(userId = userId0, addressId = 101l)

  def fetchRemoteAddress(addressId: AddressId): Address = Address(addressId)
}

object μicroservice {
  import cats.{ Monad, RecursiveTailRecM }

  def apply[F[_] : Effect : Monad : RecursiveTailRecM]: μicroservice[F] = new μicroservice[F]

  implicit def instance[F[_] : Effect : Monad : RecursiveTailRecM]: μicroservice[F] = apply[F]
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

  for {
    a <- μicroservice[Future].fetchOptionalUser(101l)
    b <- μicroservice[Future].fetchAddress(24l)
  } yield println(a,b)
}