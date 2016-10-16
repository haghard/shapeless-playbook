package examples

package object common {

  import shapeless._
  import ops.function._
  import shapeless.ops.traversable._
  import shapeless.syntax.std.traversable._

  import syntax.std.function._

  case class Record(a: Int, b: Int, c: Double, d: String)

  def fromJdbc[T, Repr <: HList](row: Array[Any])
                                (implicit gen: Generic.Aux[T, Repr], ft: FromTraversable[Repr]): Option[T] = {
    row.toHList[Repr].map(gen.from _)
  }

  def fromJdbc[P <: Product, F, L <: HList, R](p: P)(f: F)(implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]): R = {
    val hlist = gen to p
    f toProduct hlist
  }


  final class Field[T](val x: T)

  implicit def item2typeable[A: Typeable] =
    new Typeable[Field[A]] {
      override def cast(t: Any): Option[Field[A]] = t match {
        case c: Field[_] => Typeable[A].cast(c.x).map(new Field(_))
        case _ => None
      }

      override def describe: String = s"Field [${Typeable[A].describe}]"
    }


  val fields = new Field(1) :: new Field("a") :: Nil
  fields.toHList[Field[Int] :: Field[String] :: HNil]

  val row: Array[Any] = Array(1, 2, 67.89, "abc")

  fromJdbc[Record, Int :: Int :: Double :: String :: HNil](row)

  fromJdbc(1,2,5.7,"asdas")(Record(_, _, _, _))
}
