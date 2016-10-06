package examples

package object common {

  import shapeless._
  import shapeless.ops.traversable._
  import shapeless.syntax.std.traversable._

  case class Record(a: Int, b: Int, c: Double, d: String)

  def fromJdbc[T, Repr <: HList](row: Array[Any])(implicit gen: Generic.Aux[T, Repr], ft: FromTraversable[Repr]): Option[T] = {
    row.toHList[Repr].map(gen.from _)
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

}
