package examples

import scala.collection.GenTraversable

package object common {

  import shapeless._
  import ops.function._
  import shapeless.ops.traversable._
  import shapeless.syntax.std.traversable._

  import syntax.std.function._

  case class Record(a: Int, b: Int, c: Double, d: String)

  final class Field[T](val x: T)

  trait FromTraversableInd[Out <: HList] extends FromTraversable[Out] {
    def applyWithIndex(l: GenTraversable[_], ind: Int): Option[Out]
  }

  object FromTraversableInd {
    def apply[Out <: HList](implicit instance: FromTraversableInd[Out]) = instance

    //import syntax.typeable._

    implicit def hnilFromTraversable1[T] = new FromTraversableInd[HNil] {
      def apply(l: GenTraversable[_]) =
        if (l.isEmpty) Some(HNil) else None

      def applyWithIndex(l: GenTraversable[_], ind: Int) =
        if (l.isEmpty) Some(HNil) else None
    }

    implicit def hlistFromTraversable1[OutH, OutT <: HList]
    (implicit flt: FromTraversableInd[OutT], t: Typeable[OutH]) = new FromTraversableInd[OutH :: OutT] {
      def apply(l: GenTraversable[_]): Option[OutH :: OutT] =
        applyWithIndex(l, 0)

      def applyWithIndex(l: GenTraversable[_], ind: Int): Option[OutH :: OutT] =
        if (l.isEmpty) None
        else {
          println("traversable: " + ind)
          for {
            h <- {
              val r = t.cast(l.head)
              if (r.isEmpty) println(s"parse error at $ind")
              r
            } /*l.head.cast[OutH]*/
            t <- flt.applyWithIndex(l.tail, ind + 1)
          } yield {
            h :: t
          }
        }
    }
  }


  def fromJdbc[Repr <: HList, T](row: Array[Any])
                                (implicit gen: Generic.Aux[T, Repr], ft: FromTraversableInd[Repr]): Option[T] = {
    row.toHList[Repr](ft).map(gen.from _)
  }

  def fromJdbc[P <: Product, F, L <: HList, R](p: P)(f: F)(implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]): R = {
    val hlist = gen to p
    f toProduct hlist
  }

  implicit def item2typeable[A: Typeable] =
    new Typeable[Field[A]] {
      override def cast(t: Any): Option[Field[A]] = t match {
        case c: Field[_] =>
          Typeable[A].cast(c.x).map(new Field(_))
        case _ =>
          None
      }

      override def describe: String = s"Field [${Typeable[A].describe}]"
    }

  //val fields = new Field(1) :: new Field("a") :: Nil
  //fields.toHList[Field[Int] :: Field[String] :: HNil]

  val untypedRecord: Array[Any] = Array(1, 2, 67.89, "abc")

  type DbRecord = Int :: Int :: Double :: String :: HNil

  fromJdbc[DbRecord, Record](untypedRecord)

  fromJdbc(1, 2, 5.7, "bla-bla")(Record(_, _, _, _))
}
