package examples.catsfree.macros

import cats.Id
import cats.free.Free
import freasymonad.free

//examples.catsfree.macros.kvstoreWithMacros
//https://github.com/Thangiee/Freasy-Monad
object kvstoreWithMacros {

  @free trait KVStoreOps {

    // as long as you define a type alias for Free
    sealed trait GrammarADT[A]

    // you can use any names you like
    type KVStoreF[A] = Free[GrammarADT, A]

    // and a sealed trait.

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
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
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

  impureInterpreter.run(program)
}