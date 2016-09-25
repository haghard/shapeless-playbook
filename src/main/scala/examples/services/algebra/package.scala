package examples.services

import cats.data.{XorT, Xor}

package object algebra {
  import cats._

  sealed abstract class ServiceOp[A] extends Product with Serializable
  final case class FetchUser(userId: Long) extends ServiceOp[TimeoutException Xor User]
  final case class FetchAddress(addressId: Long) extends ServiceOp[TimeoutException Xor Address]

  type ServiceIO[A] = cats.free.Free[ServiceOp, A]

  object ServiceOps {
    def fetchUser(userId: Long): ServiceIO[TimeoutException Xor User] =
      cats.free.Free.liftF(FetchUser(userId))

    def fetchAddress(addressId: Long): ServiceIO[TimeoutException Xor Address] =
      cats.free.Free.liftF(FetchAddress(addressId))
  }

  def interpreter[M[_] : Effect : Monad : RecursiveTailRecM](implicit ins: μservice[M]): ServiceOp ~> M =
    new (ServiceOp ~> M) {
      override def apply[A](fa: ServiceOp[A]): M[A] = {
        val result = fa match {
          case FetchUser(userId) => (ins fetchUser userId)
          case FetchAddress(addressId) => (ins fetchAddress addressId)
        }
        result.asInstanceOf[M[A]]
      }
    }

  def fetchBoth(userId: Long): ServiceIO[TimeoutException Xor (User, Address)] =
    (for {
      user <- XorT[ServiceIO, TimeoutException, User](ServiceOps.fetchUser(userId))
      address <- XorT[ServiceIO, TimeoutException, Address](ServiceOps.fetchAddress(user.addressId))
    } yield (user, address)).value
}