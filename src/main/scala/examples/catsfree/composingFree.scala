package examples.catsfree

import cats.data.Coproduct
import cats.free.{Free, Inject}
import cats.{Id, ~>}

object composingFree {
  /**********************************************/
  sealed trait RW[T]
  case class Write[T](result: T) extends RW[T]
  case class Read[T](prompt: String, parse: String => T) extends RW[T]

  /**********************************************/
  sealed trait KVOps[T]
  case class Put[T](key: String, value: T) extends KVOps[Unit]
  case class Get[T](key: String) extends KVOps[Option[T]]
  case class Delete(key: String) extends KVOps[Unit]

  type AppDsl[A] = Coproduct[RW, KVOps, A]

  class RWInteracts[F[_]](implicit I: Inject[RW, F]) {
    def readDouble: Free[F, Double] =
      Free.inject[RW, F](Read[Double]("> enter double: ", { _.toDouble }))

    def readInt: Free[F, Int] =
      Free.inject[RW, F](Read[Int]("> enter int: ", { _.toInt }))

    def readKey: Free[F, String] =
      Free.inject[RW, F](Read[String]("> enter a key: ", identity))
  }

  object RWInteracts {
    implicit def interacts[F[_]](implicit I: Inject[RW, F]): RWInteracts[F] =
      new RWInteracts[F]
  }

  class KVStore[F[_]](implicit I: Inject[KVOps, F]) {
    def put[T](key: String, value: T): Free[F, Unit] =
      Free.inject[KVOps, F](Put[T](key, value))

    def get[T](key: String): Free[F, Option[T]] =
      Free.inject[KVOps, F](Get[T](key))
  }

  object KVStore {
    implicit def dataSource[F[_]](implicit I: Inject[KVOps, F]): KVStore[F] =
      new KVStore[F]
  }

  object ConsoleInterpreter extends (RW ~> Id) {
    def apply[A](i: RW[A]) = i match {
      case Read(prompt, parse) =>
        val in = parse(scala.io.StdIn.readLine(prompt))
        println("\n" + in)
        in
      case Write(msg) =>
        println(msg)
        msg
    }
  }

  object MapInterpreter extends (KVOps ~> Id) {
    val kvs = scala.collection.mutable.Map.empty[String, Any]
    def apply[A](fa: KVOps[A]): Id[A] =
      fa match {
        case Put(key, value) =>
          println(s"put($key, $value)")
          kvs(key) = value
          ()
        case Get(key) =>
          println(s"get($key)")
          kvs.get(key).map(_.asInstanceOf[A])
      }
  }

  def program(implicit I: RWInteracts[AppDsl], D: KVStore[AppDsl]) = {
    import D._
    import I._

    for {
      key <- readKey
      value <- readDouble
      _ <- put[Double](key, value)
      x <- get[Double](key)
    } yield (x)
  }

  val interpreter: AppDsl ~> Id = ConsoleInterpreter or MapInterpreter

  val res = program.foldMap(interpreter)
  //res

}
