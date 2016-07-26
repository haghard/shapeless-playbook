package examples.parser

object TypeClassParser {

  case class User(id: Int, name: String, email: String)
  case class User1(id: Int, name: String, email: String)

  trait Readable[T] {
    def read(line: String): T
  }

  object Readable {

    def line2Readable[T](parser: String => T) = new Readable[T] {
      override def read(x: String): T = parser(x)
    }

    def apply[A: Readable]: Readable[A] = implicitly[Readable[A]]

    implicit def readableDouble(f: String => Double) =
      line2Readable[Double](f(_))
    implicit val ReadableInt = line2Readable[Int](_.toInt)
    implicit val ReadableLong = line2Readable[Long](_.toLong)
    implicit val ReadableString = line2Readable[String](new String(_))
    implicit val ReadableBoolean = line2Readable[Boolean](_.toBoolean)
    implicit val ReadableCharList =
      line2Readable[List[Char]](_.toCharArray.toList)
    implicit val ReadableStringList =
      line2Readable[List[String]](_.split(':').toList)

    implicit val readableUser: Readable[User] = line2Readable(
        _.split(':') match {
      case Array(id, name, email) =>
        User(id.read[Int], name.read[String], email.read[String])
      case fields =>
        throw new Exception(s"Expected fields size:3 - actual: ${fields.size}")
    })

    implicit class Ops(val line: String) {
      def read[T: Readable]: T = (implicitly[Readable[T]] read line)
    }
  }

  def main(args: Array[String]) = {
    import Readable._
    import shapeless._

    //http://rnduja.github.io/2016/01/19/a_shapeless_primer/
    val G = Generic[User]
    val user = G.from(199 :: "haghard" :: "haghad@gmail.com" :: HNil)
    println(user)

    /*
    import scalaz.Kleisli._
    import scalaz._, Scalaz._
    val component = for {
      a <- kleisli { (x: String) ⇒ scala.util.Try(x.toInt).map(_ + 2).toOption }
      b <- kleisli { (x: String) ⇒ scala.util.Try(x.toInt).map(_ * 2).toOption }
    } yield (a + b)
    println(component run "99")
     */

    println("9945".read[Int])
    println(Readable[Int].read("994589"))
    println("345343:haghard:haghad@gmail.com".read[User])

    //println("345343:haghard:haghad@gmail.com".read[User1])
  }
}
