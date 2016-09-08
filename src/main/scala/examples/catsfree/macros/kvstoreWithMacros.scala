package examples.catsfree.macros

import cats.Id
import cats.free.Free
import freasymonad.free

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

//examples.catsfree.macros.kvstoreWithMacros
//https://github.com/Thangiee/Freasy-Monad
object kvstoreWithMacros {

  @free trait KVStoreOps {

    sealed trait GrammarADT[A]

    type KVStoreF[A] = Free[GrammarADT, A]

    // abstract methods are automatically lifted into part of the grammar ADT
    def put[T](key: String, value: T): KVStoreF[Unit]

    def get[T](key: String): KVStoreF[Option[T]]

    def delete(key: String): KVStoreF[Unit]

    def update[T](key: String, f: T => T): KVStoreF[Unit] =
      for {
        vMaybe <- get[T](key)
        _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
      } yield ()
  }

  import KVStoreOps.ops._

  def program: KVStoreF[Option[Int]] =
    for {
      _ <- put("key-a", 2)
      _ <- update[Int]("key-a", _ + 12)
      _ <- put("key-b", 5)
      n <- get[Int]("key-c")
      _ <- delete("key-b")
    } yield n

  val impureInterpreter = new KVStoreOps.Interp[Id] {
    val kvs = scala.collection.mutable.Map.empty[String, Any]

    def get[T](key: String): Id[Option[T]] = {
      println(s"get($key)")
      kvs.get(key).map(_.asInstanceOf[T])
    }

    def put[T](key: String, value: T): Id[Unit] = {
      println(s"put($key, $value)")
      kvs(key) = value
    }

    def delete(key: String): Id[Unit] = {
      println(s"delete($key)")
      kvs.remove(key)
    }
  }

  val impureFutureInterpreter = new KVStoreOps.Interp[Future] {
    val kvs = scala.collection.mutable.Map.empty[String, Any]

    def get[T](key: String): Future[Option[T]] = Future {
      println(s"get($key)")
      kvs.get(key).map(_.asInstanceOf[T])
    }

    def put[T](key: String, value: T): Future[Unit] = Future {
      println(s"put($key, $value)")
      kvs(key) = value
    }

    def delete(key: String): Future[Unit] = Future {
      println(s"delete($key)")
      kvs.remove(key)
    }
  }


  import cats.instances.all._
  import cats.syntax.flatMap._

  impureInterpreter.run(program)
  impureFutureInterpreter.run(program)
}