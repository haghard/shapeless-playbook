package examples.parser

import cats.Monoid
import cats.std.all._
import cats.syntax.all._
import shapeless.UnaryTCConstraint._
import shapeless._
import shapeless.ops.hlist._
import shapeless.poly._

import scala.annotation.tailrec
import scala.util.Try

//https://github.com/lancelet/typequest/blob/master/src/main/scala/typequest/SimpleParse.scala

//examples.parser.PolyFunctionsParser
object PolyFunctionsParser {

  case class Parser[T](parse: String => Option[T])

  val strParser = Parser[String](Option(_))
  val intParser = Parser[Int](x => Try(x.toInt).toOption)
  val doubleParser = Parser[Double](x => Try(x.toDouble).toOption)

  object polyFunction extends Poly1 {
    implicit def parse[T] = at[(Parser[T], String)] {
      case (parser, line) => (parser parse line)
    }
  }

  val hParser = strParser :: doubleParser :: HNil

  def runParsers[PS <: HList, H0 <: HList, H1 <: HList](parsers: PS)(
      line: String)(implicit parserConstraint: *->*[Parser]#λ[PS],
                    constMapper: ConstMapper.Aux[String, PS, H0],
                    zipper: Zip.Aux[PS :: H0 :: HNil, H1],
                    mapper: Mapper[polyFunction.type, H1]): mapper.Out = {
    val cells = parsers mapConst line
    (parsers zip cells) map polyFunction
  }

  def runParsersAll[PS <: HList, H0 <: HList, H1 <: HList](parsers: PS)(
    tokens: List[String])(
      implicit parserConstraint: *->*[Parser]#λ[PS],
      constMapper: ConstMapper.Aux[String, PS, H0],
      zipper: Zip.Aux[PS :: H0 :: HNil, H1],
      mapper: Mapper[polyFunction.type, H1]): Map[String, mapper.Out] = {
    @tailrec
    def loop(parsers: PS,
             in: List[String],
             map: Map[String, mapper.Out]): Map[String, mapper.Out] = {
      if (!in.isEmpty) {
        val token = in.head
        val cells = parsers.mapConst(token)
        //((parsers zip cells) map polyFunction).map(singletonMap)
        loop(parsers,
             in.tail,
             map + (token -> ((parsers zip cells) map polyFunction)))
      } else map
    }
    loop(parsers, tokens, Map[String, mapper.Out]())
  }

  val out = runParsers(hParser)("Hello")
  println(out)

  val outAll = runParsersAll(hParser)(List("Hello", "1.9"))
  println(outAll)

  /**
    * Construct a single-element map for each parsed result
    * This is:  forall T. Option[T] => Map[T, Long]
    */
  object singletonMap extends (Option ~> ({ type λ[T] = Map[T, Long] })#λ) {
    def apply[T](x: Option[T]) = x match {
      case Some(v) => Map(v -> 1L)
      case None => Map.empty
    }
  }

  val singletonMapExample = runParsers(hParser)("qwerty") map singletonMap
  println(singletonMapExample)

  object HLMonoid extends ProductTypeClassCompanion[Monoid] {

    object typeClass extends ProductTypeClass[Monoid] {
      override def emptyProduct =
        new Monoid[HNil] {
          def empty = HNil

          def combine(a: HNil, b: HNil) = HNil
        }

      override def product[F, T <: HList](mh: Monoid[F], mt: Monoid[T]) =
        new Monoid[F :: T] {
          def empty = mh.empty :: mt.empty

          def combine(a: F :: T, b: F :: T) =
            mh.combine(a.head, b.head) :: mt.combine(a.tail, b.tail)
        }

      override def project[F, G](instance: => Monoid[G],
                                 to: F => G,
                                 from: G => F) =
        new Monoid[F] {
          def empty = from(instance.empty)

          def combine(a: F, b: F) = from(instance.combine(to(a), to(b)))
        }
    }

  }

  import HLMonoid._

  def histograms[PS <: HList,
                 H0 <: HList,
                 H1 <: HList,
                 H2 <: HList,
                 H3 <: HList](parserHList: PS)(columns: List[String])(
      implicit parserConstrant: *->*[Parser]#λ[PS],
      ev1: ConstMapper.Aux[String, PS, H0],
      ev2: Zip.Aux[PS :: H0 :: HNil, H1],
      ev3: Mapper.Aux[polyFunction.type, H1, H2],
      ev4: Mapper.Aux[singletonMap.type, H2, H3],
      ev5: Monoid[H3]) =
    columns
      .map(column => runParsers(parserHList)(column) map singletonMap)
      .combineAll

  val column = List("Hello", "42.0", "True", "False", "True", "True", "False", "True", "41", "42")
  val tvhExample = histograms(hParser)(column)
  println(tvhExample)


  /*******************************************************************************************************/

  type Tweet = Int
  type ValidTweet = scalaz.ValidationNel[String, Tweet]

  type Record = String
  type ValidRecord = scalaz.ValidationNel[String, Record]

  import shapeless._, poly._

  type PT[T] = scalaz.ValidationNel[String, T]
  object toOption extends (PT ~> scala.Option) {
    def apply[T](result: PT[T]) = result.fold({ errors => None }, { t => Some(t) })
  }


  //val hResult = ValidTweet :: ValidRecord :: ValidTweet :: HNil
  import scalaz._, Scalaz._
  val results = 23.successNel[String] :: "foo".successNel[String] :: 45.successNel[String] :: shapeless.HNil

  val resultsWithError = 23.successNel[String] :: "foo".successNel[String] :: "error fetching 3th element".failureNel[Int] :: shapeless.HNil

  results map toOption

  resultsWithError map toOption
}
