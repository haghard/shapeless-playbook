package examples

import examples.parser.PolyFunctionsParser.polyFunction._
import shapeless.PolyDefns.~>
import shapeless._

//https://gist.github.com/Lasering/bd997ef0f0ffb9a030fca0c5f6f65f90
package object stages {

  sealed trait StageComapped[L <: HList, M <: HList]

  object StageComapped {
    implicit def nil: StageComapped[HNil, HNil] = new StageComapped[HNil, HNil] {}
    implicit def cons[Head, Tail <: HList, ResultsTail <: HList](implicit tc: StageComapped[Tail, ResultsTail]) =
      new StageComapped[Stage[Head] :: Tail, Head :: ResultsTail] {}
  }

  abstract class Stage[R](val value: R) {
    def weight: Int
  }

  object f extends Poly1 {
    implicit def cse = at[Int](a => a * 2)
  }

  implicit class hlist2Relation[A <: HList, B <: HList](dependencies: A)(implicit ev: StageComapped[A, B]) {
    def fanOut[R](f: B => Stage[R]) = ???
      //dependencies.map
      //println("1")
  }


  val a = new Stage("A") {  override def weight = 9 }
  val b: Stage[String] = new Stage("B") { override def weight = 9 }

  val aHList: ::[Stage[String] { def weight: Int }, HNil] = a :: HNil
  val bHList: ::[Stage[String], HNil] = b :: HNil


  val aD = (a :: HNil).fanOut { case s :: HNil =>
    new Stage(s.length) { override def weight = value * 5  }
  }

  val bD = (b :: HNil).fanOut { case s :: HNil =>
    new Stage(s.length) {
      override def weight = value * 2
    }
  }

  (a :: b :: HNil).fanOut { case a0 :: b0 :: HNil =>
    new Stage("C") {
      override def weight = a0.length + b0.length + value.length
    }
  }

  //https://gist.github.com/tek/17f46e8da5243b31177b718c9d0f2fc2
}
