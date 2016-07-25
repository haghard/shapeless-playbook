package examples.free

import scalaz.Free.FreeC
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

//http://kanaka.io/blog
//http://rnduja.github.io/2016/01/19/a_shapeless_primer/
package object free {

  //Algebra
  sealed trait Console[T]
  case class WriteValue[T](result: T) extends Console[T]
  case class ReadValue[T](prompt: String, parse: String => T) extends Console[T]

  type Dsl[T] = FreeC[Console, T]

  trait Interpretator[F[_]] { self ⇒
    def evaluate[T](given: F[T]): T
    def ~>[G[_] : Monad]: (F ~> G) = Interpretator.nat(self, scalaz.Monad[G])
  }

  object Interpretator {
    def apply[F[_] : Interpretator] = implicitly[Interpretator[F]]

    private[Interpretator] def nat[F[_], G[_], E <: Interpretator[F]](implicit E: E, G: Monad[G]) = new (F ~> G) {
      override def apply[A](console: F[A]): G[A] =
        (G.pure(E evaluate console))
    }
  }

  object StdInInterp extends Interpretator[Console] {
    var count = 0

    override def evaluate[T](given: Console[T]): T = {
      given match {
        //Side effect
        //println(s"Result: $result")
        case r: WriteValue[T] =>
          //println()
          count += 1
          //println("cnt:" + count)
          r.result
        case r: ReadValue[T] =>
          count += 1
          val in = r.parse(scala.io.StdIn.readLine(r.prompt))
          //res.getClass.getName
          println("\n" + in)
          in
      }
    }
  }

  def readDouble: Dsl[Double] = Free.liftFC(ReadValue[Double]("> enter a double: ", { _.toDouble  }))

  def readInt: Dsl[Int] = Free.liftFC(ReadValue[Int]("> enter an int: ", {  _.toInt }))

  def readString: Dsl[String] = Free.liftFC(ReadValue[String]("> enter a string: ", identity))

  def randomString: Dsl[String] = Free.liftFC(WriteValue[String](java.util.UUID.randomUUID.toString))

  import shapeless._
  import ops.function._
  import syntax.std.function._

  /**
    *
    * Let be P a Product, that is, a tuple or a case class
    * Let be F an unconstrained type parameter
    * Let be L an HList
    * Let be R an unconstrained type parameter

    * Generic.Aux[P, L]; this is the built-in “predicate” that Shapeless provides to encode the relation between a product type P and an HList L.
    * It holds when it is possible to derive a Generic[P] instance that converts P into L

    * FnToProduct.Aux[F, L => R]; is he built-in “predicate” that Shapeless provides to encode the relation that holds
    * when F can be converted into a function from HList L to return type R; it holds when it is possible
    * to derive an FnToProduct[F] instance called that converts F into L => R
    *
    * HLists can be seen as an alternative implementation of the concept of Tuple or more generally, of the concept of Product.
    * Abstracting over Arity
    */
  def product[P <: Product, F, L <: HList, R](p: P)(f: F)(implicit generic: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]): Dsl[R] = {
    val hlist = generic to p
    Free.liftFC(WriteValue[R](f toProduct hlist))
  }

  def productH[P <: Product, L <: HList](args: P)(implicit generic: Generic.Aux[P, L]): L = (generic to args)
}

/**
  * sbt console
  * examples.free.FreeApplication.main(Array("1"))
  */
object FreeApplication extends App {
  import free._
  import shapeless._

  case class Result(a: Int, b: String, c: Double)

  val InputOutputProgram =
    for {
      int ← readInt
      double ← readDouble
      line ← randomString
      r ← product(int, line, double)(Result(_, _, _))
    } yield r


  val multiplyBy2Program = for {
    a ← readInt
    b ← readInt
    r ← product(a, b)((_: Int) * 2 + (_: Int) * 2)
  } yield r

  implicit val Id = free.StdInInterp.~>[Id]
  implicit val Op = free.StdInInterp.~>[Option]
  implicit val Task = free.StdInInterp.~>[Task]

  println("★ ★ ★ InputOutputProgram with Id as an effect ★ ★ ★")
  val res = Free.runFC(InputOutputProgram)(Id)
  println("> " + res)

  println("★ ★ ★ MultiplyBy2Program with Id as an effect ★ ★ ★")
  val res1 = Free.runFC(multiplyBy2Program)(Id)
  println("> " + res1)

  println("★ ★ ★ InputOutputProgram with Option as an effect ★ ★ ★")
  val res2 = (Free.runFC(InputOutputProgram)(Op))
  println("> " + res2)

  //println(examples.free.free.product(1, "fghfg", 5.6d, 'c, 23))

  println("★ ★ ★ InputOutputProgram with Task as an effect ★ ★ ★")
  val res3 = (Free.runFC(InputOutputProgram)(Task)).attemptRun
  println("> " + res3)
}