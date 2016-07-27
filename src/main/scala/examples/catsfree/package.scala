package examples

import cats.free.Free

package object catsfree {
  import cats.{Id, ~>}
  import scala.collection.mutable

  sealed trait KVOps[T]
  case class Put[T](key: String, value: T) extends KVOps[Unit]
  case class Get[T](key: String) extends KVOps[Option[T]]
  case class Delete(key: String) extends KVOps[Unit]

  type KVDsl[A] = Free[KVOps, A]

  //smart constructors
  def put[T](key: String, value: T): KVDsl[Unit] =
    Free.liftF[KVOps, Unit](Put[T](key, value))

  def get[T](key: String): KVDsl[Option[T]] =
    Free.liftF[KVOps, Option[T]](Get[T](key))

  def delete(key: String): KVDsl[Unit] = Free.liftF(Delete(key))
  // Update composes get and set, and returns nothing.
  def update[T](key: String, f: T => T): KVDsl[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()

  def compareAndSet[T](key: String, f: T => T): KVDsl[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()

  implicit val optionApp: cats.Apply[Option] = new cats.Apply[Option] {
    override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
      fa.flatMap(a => f.map(ff => ff(a)))
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }

  def program: KVDsl[Option[Double]] =
    for {
      _ <- put("key_a", 2)
      _ <- update[Int]("key_a", (_ + 12))
      _ <- put("key_b", 5.1)
      a <- get[Int]("key_a")
      b <- get[Double]("key_b")
    } yield {
      cats.Apply[Option].map2(a, b)(_ + _)
      /*a.flatMap(x => b.map(y => x + y))*/
    }

  def transformationId: KVOps ~> Id =
    new (KVOps ~> Id) {
      val kvs = mutable.Map.empty[String, Any]
      def apply[A](fa: KVOps[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).map(_.asInstanceOf[A])
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }
    }

  val result = program.foldMap(transformationId)
  println(result)
}
