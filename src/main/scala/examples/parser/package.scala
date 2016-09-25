package examples

//https://meta.plasm.us/posts/2015/11/08/type-classes-and-generic-derivation/
//http://stackoverflow.com/questions/33858745/automatically-convert-a-case-class-to-an-extensible-record-in-shapeless/33860864#33860864
package object parser {

  import scala.reflect.ClassTag
  import scala.util.Try

  object JavaReflectionParser {
    def apply[T: ClassTag](s: String): Option[T] = Try {
      val ctor = implicitly[ClassTag[T]].runtimeClass.getConstructors.head
      val paramsArray = s.split(",").map(_.trim)
      val paramsWithTypes = paramsArray.zip(ctor.getParameterTypes)

      val parameters = paramsWithTypes.map {
        case (param, cls) => cls.getName match {
          case "int" => param.toInt.asInstanceOf[Object]
          case "double" => param.toDouble.asInstanceOf[Object]
          case "string" => param.toString.asInstanceOf[Object]
          case _ =>
            val paramConstructor = cls.getConstructor(param.getClass)
            paramConstructor.newInstance(param).asInstanceOf[Object]
        }
      }

      ctor.newInstance(parameters: _*).asInstanceOf[T]
    }.toOption
  }



  import scala.util.Try
  import shapeless._
  import scalaz._, Scalaz._


  trait Parser[A] {
    def apply(line: String): Option[A]
  }

  implicit val stringParser: Parser[String] = new Parser[String] {
    def apply(s: String) = Some(s)
  }

  implicit val intParser: Parser[Int] = new Parser[Int] {
    def apply(s: String) = Try(s.toInt).toOption
  }

  implicit val doubleParser: Parser[Double] = new Parser[Double] {
    def apply(s: String) = Try(s.toDouble).toOption
  }

  //This says that we know how to parse a string into an empty HList (i.e. HNil)
  implicit val hNilParser: Parser[HNil] = new Parser[HNil] {
    def apply(s: String): Option[HNil] = if (s.isEmpty) Some(HNil) else None
  }

  //We know how to parse a string into an HList made up of a head H and a tail T, but only if we know how to parse into both H and T
  implicit def hConsParser[H: Parser, T <: HList: Parser]: Parser[H :: T] =
    new Parser[H :: T] {
      def apply(line: String): Option[H :: T] = {
        (line split ";").toList match {
          case h +: rest =>
            for {
              head <- implicitly[Parser[H]].apply(h)
              tail <- implicitly[Parser[T]].apply(rest mkString (";"))
              //_ = { println(line); println(head); println(tail); println("******") }
            } yield head :: tail
        }
      }
    }

  //https://stackoverflow.com/questions/33725935/shapeless-generic-aux/33738732#33738732
  //gen.Repr  Generic.Aux[T, gen.Repr]
  def parse[T](obj: T)(implicit gen: Generic[T]): gen.Repr = {
    val genView: gen.Repr = (gen to obj)
    val obj0: T = (gen from genView)
    println(s"Generic: $genView")
    println(s"Case class: $obj0")
    genView
  }

  /*Generic[A] { type Repr = R }*/
  implicit def caseClassParser[A, R <: HList](implicit Gen: Generic.Aux[A, R], parser: Parser[R]) =
    new Parser[A] {
      def apply(line: String): Option[A] = {
        (parser(line) map (Gen.from))
      }
    }

  object Parser {
    def apply[A: Parser](s: String): Option[A] =
      implicitly[Parser[A]].apply(s)
  }

  case class Person(name: String, age: Double)
  case class Book(title: String, author: String, year: Int)
  case class Country(name: String, population: Int, area: Double)

  parse(Book("Hamlet","Shakespeare",2012))

  Parser[Book]("Hamlet;Shakespeare;2012") //Some
  //Parser[Book]("Hamlet;Shakespeare") //None
  //parser.Parser[Country]("England;12f;13.6a")
}
