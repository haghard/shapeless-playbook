package examples.catsfree

import cats.{Id, ~>}
import cats.free.Free
import scala.collection.mutable

//examples.catsfree.catsfree
object catsfree {

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

  def delete(key: String): KVDsl[Unit] =
    Free.liftF(Delete(key))

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

  /*implicit val optionApp: cats.Apply[Option] = new cats.Apply[Option] {
    override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
      fa.flatMap(a => f.map(ff => ff(a)))
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }*/
  import cats.instances.all._

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

  //this interpreter is impure -- it mutates kvs and also produces logging output using println.
  def interpreterId: KVOps ~> Id =
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

  //program.foldMap(transformationId)

  import cats.data.State
  type Store = mutable.Map[String, Any]
  type KVStoreState[T] = State[mutable.Map[String, Any], T]

  def transformationState: KVOps ~> KVStoreState =
    new (KVOps ~> KVStoreState) {
      def apply[A](fa: KVOps[A]): KVStoreState[A] =
        fa match {
          case p: Put[A] =>
            //short version State.modify(_.updated(p.key, p.value))
            State[Store, A] { kvs: Store =>
              println(s"put(${p.key}, ${p.value})")
              (kvs.updated(p.key, p.value), ())
            }
          case g: Get[A] =>
            //short version State.inspect(_.get(g.key).map(_.asInstanceOf[A]))
            State { kvs: Store =>
              println(s"get(${g.key})")
              (kvs, kvs.get(g.key).map(_.asInstanceOf[A]))
            }
          case Delete(key) =>
            State[Store, A] { kvs: Store =>
              println(s"delete(${key})")
              kvs.remove(key)
              (kvs, ().asInstanceOf[A])
            }
        }
    }

  /***********************************************************/
  val stateProgram: KVDsl[Option[Double]] = for {
    _ <- put("key_a", 2)
    _ <- update[Int]("key_a", (_ + 12))
    _ <- put("key_b", 5.1)
    a <- get[Int]("key_a")
    b <- get[Double]("key_b")
  } yield (cats.Apply[Option].map2(a, b)(_ + _))

  val initState = mutable.Map[String, Any]()
  val state = stateProgram.foldMap(transformationState)
  val res = (state run initState)
  //res.value

  /***********************************************************/
  def putP[A](key: String, value: A) =
    State[Store, A] { kvs: Store =>
      (kvs.updated(key, value), value)
    }

  def getP[A](key: String) =
    State[Store, A] { kvs: Store =>
      (kvs,
       kvs.get(key).map(_.asInstanceOf[A]).getOrElse(null.asInstanceOf[A]))
    }

  val prog = for {
    _ <- putP("a", 12)
    _ <- putP("b", 8)
    a <- getP[Int]("a")
    b <- getP[Int]("b")
  } yield { a + b }

  val res0 = prog.run(mutable.Map[String, Any]())
  //res0.value
}
