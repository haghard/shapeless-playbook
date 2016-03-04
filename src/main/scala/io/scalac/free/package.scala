package io.scalac

import scalaz.Free.FreeC
import scalaz._, Scalaz._
import scalaz.concurrent.Task

package object free {

  sealed trait Console[T]

  case class Output[T](result: T) extends Console[T]
  case class ReadValue[T](prompt: String, parser: String => T) extends Console[T]

  type Dsl[T] = FreeC[Console, T]

  trait Interpretator[F[_]] { self ⇒
    def evaluate[T](given: F[T]): T
    def ~>[G[_]: Monad]: (F ~> G) = Interpretator.nat(self, scalaz.Monad[G])
  }

  object Interpretator {
    def apply[F[_]: Interpretator] = implicitly[Interpretator[F]]

    private[Interpretator] def nat[F[_], G[_], E <: Interpretator[F]](implicit E: E, G: Monad[G]) = new (F ~> G) {
      override def apply[A](console: F[A]): G[A] =
        (G.pure(E evaluate console))
    }
  }

  object Default extends Interpretator[Console] {
    var state = 0

    override def evaluate[T](given: Console[T]): T = {
      given match {
        //Side effect
        //println(s"Result: $result")
        case r: Output[T] =>
          println(state)
          state = state + 1
          r.result
        case r: ReadValue[T] =>
          println(state)
          state = state + 1
          val res = r.parser(scala.io.StdIn.readLine(r.prompt))
          res.getClass.getName
          res
      }
    }
  }

  def readDouble: Dsl[Double] = Free.liftFC(ReadValue[Double]("Please enter you double: ", { _.toDouble }))

  def readInt: Dsl[Int] = Free.liftFC(ReadValue[Int]("Please enter you int: ", { _.toInt }))

  def readString: Dsl[String] = Free.liftFC(ReadValue[String]("Please enter you string: ", identity))

  def randomString: Dsl[String] = Free.liftFC(Output[String](java.util.UUID.randomUUID().toString))

  //def map2[T, E, A](a: T, b: E)(f:(T,E) => A): Dsl[A] = Free.liftFC(Output[A](f(a, b)))
  //def map3[T, E, O, A](a: T, b: E, c: O)(f:(T,E,O) => A): Dsl[A] = Free.liftFC(Output[A](f(a, b, c)))

  //http://kanaka.io/blog
  //http://rnduja.github.io/2016/01/19/a_shapeless_primer/
  import shapeless._
  import syntax.std.function._
  import ops.function._

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
  def product[P <: Product, F, L <: HList, R](p: P)(f: F)(implicit generic: Generic.Aux[P, L],
                                                          fp: FnToProduct.Aux[F, L => R]):Dsl[R] = {
    val hlist = generic to p
    Free.liftFC(Output[R](f toProduct hlist))
  }

  def productH[P <: Product, L <: HList](args: P)(implicit generic: Generic.Aux[P, L]): L = (generic to args)
}

object Runner extends App {
  import free._
  import shapeless._

  case class Result(a: Int, b: String, c: Double)

  val program =
    for {
      int ← readInt
      line ← randomString
      double <- readDouble
      r ← product(int, line, double)(Result(_,_,_))
    } yield r


  val program1 = for {
    a <- readInt
    b <- readInt
    r ← product(a, b)((_:Int) * 2 + (_:Int) * 2)
  } yield r

  implicit val Id = free.Default.~>[Id]
  implicit val Op = free.Default.~>[Option]
  implicit val Task = free.Default.~>[Task]

  println((Free.runFC(program)(Id)))
  println((Free.runFC(program1)(Id)))

  //println((Free.runFC(program)(Op)))
  //println(Free.runFC(program)(Task).run)

  //println(productH(1, "fghfg", 5.6d, 'c))

  //def runTask = Free.runFC(program)(EvaluatorLogic.~>[Task])
  //runTask.run
}