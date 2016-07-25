package examples.parser

import scala.annotation.tailrec

//https://github.com/lancelet/typequest/blob/master/src/main/scala/typequest/SimpleParse.scala
object PolyFunctionsParser {

  import shapeless._
  import UnaryTCConstraint._
  import cats.Monoid
  import cats.std.all._
  import cats.syntax.all._
  import ops.hlist._
  import poly._

  import scala.util.Try

  case class Parser[T](parse: String => Option[T])

  val strParser = Parser[String](Option(_))
  val intParser = Parser[Int](x => Try(x.toInt).toOption)
  val doubleParser = Parser[Double](x => Try(x.toDouble).toOption)

  object polyFunction extends Poly1 {
    implicit def parse[T] = at[(Parser[T], String)] { case (parser, line) => (parser parse line) }
  }

  val hParser = strParser :: doubleParser :: HNil

  def runParsers[PS <: HList, H0 <: HList, H1 <: HList](parsers: PS)(line: String)(implicit parserConstraint: *->*[Parser]#λ[PS],
                                                                                   constMapper: ConstMapper.Aux[String, PS, H0],
                                                                                   zipper: Zip.Aux[PS :: H0 :: HNil, H1],
                                                                                   mapper: Mapper[polyFunction.type, H1]): mapper.Out = {
    val cells = parsers mapConst line
    (parsers zip cells) map polyFunction
  }

  def runParsersAll[PS <: HList, H0 <: HList, H1 <: HList](parsers: PS)(in: List[String])(implicit parserConstraint: *->*[Parser]#λ[PS],
                                                                                          constMapper: ConstMapper.Aux[String, PS, H0],
                                                                                          zipper: Zip.Aux[PS :: H0 :: HNil, H1],
                                                                                          mapper: Mapper[polyFunction.type, H1]): Map[String, mapper.Out] = {
    @tailrec
    def loop(parsers: PS, in: List[String], out: Map[String, mapper.Out]): Map[String, mapper.Out] = {
      if(!in.isEmpty) {
        val el = in.head
        val cells = parsers.mapConst(el)

        ((parsers zip cells) map polyFunction).map(singletonMap)
        loop(parsers, in.tail, out + (el -> ((parsers zip cells) map polyFunction)))
      } else out
    }

    loop(parsers, in, Map[String, mapper.Out]())
  }

  val out = runParsers(hParser)("Hello")
  println(out)

  val outAll = runParsersAll(hParser)(List("Hello", "1.9"))
  println(outAll)

  /**
    * Construct a single-element map for each parsed result
    * This is:  forall T. Option[T] => Map[T, Long]
    */
  object singletonMap extends (Option ~> ({type L[T] = Map[T, Long]})#L) {
    def apply[T](x: Option[T]) = x match {
      case Some(y) => Map(y -> 1L)
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
          def combine(a: F :: T, b: F :: T) = mh.combine(a.head, b.head) :: mt.combine(a.tail, b.tail)
        }

      override def project[F, G](instance: => Monoid[G], to: F => G, from: G => F) =
        new Monoid[F] {
          def empty = from(instance.empty)
          def combine(a: F, b: F) = from(instance.combine(to(a), to(b)))
        }
    }

  }

  import HLMonoid._

  def histograms[PS <: HList, H0 <: HList, H1 <: HList, H2 <: HList, H3 <: HList](parserHList: PS)(columns: List[String])
                                                                                 (implicit parserConstrant: *->*[Parser]#λ[PS],
                                                                                  ev1: ConstMapper.Aux[String, PS, H0],
                                                                                  ev2: Zip.Aux[PS :: H0 :: HNil, H1],
                                                                                  ev3: Mapper.Aux[polyFunction.type, H1, H2],
                                                                                  ev4: Mapper.Aux[singletonMap.type, H2, H3],
                                                                                  ev5: Monoid[H3]) = columns.map(column => runParsers(parserHList)(column) map singletonMap).combineAll

  val column = List("Hello", "42.0", "True", "False", "True", "True", "False", "True", "41", "42")
  val tvhExample = histograms(hParser)(column)
  println(tvhExample)
}