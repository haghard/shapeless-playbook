package examples.parser

import org.scalatest.{FlatSpec, Matchers}
import shapeless.HNil
import scalaz.{Failure, NonEmptyList, Success}

class ParsersSpec extends FlatSpec with Matchers {

  "Parser" should "succeeds" in {
    Parser[Book]("Hamlet;Shakespeare;2012") should equal(Success(Book("Hamlet", "Shakespeare", 2012)))
  }

  "Parser" should "fails because of the absence of one field" in {
    Parser[Book]("Hamlet;Shakespeare") should equal(Failure(NonEmptyList("Error parsing Int : For input string: \"\"")))
  }

  "Parser" should "fails because of wrong types" in {
    Parser[Country]("England;12f;13.6a") should equal(Failure(NonEmptyList("Error parsing Int 12f: For input string: \"12f\"")))
  }

  "Case class" should "be converted into HList" in {
    parse[Book](Book("Hamlet", "Shakespeare", 2012)) should(equal("Hamlet" :: "Shakespeare" :: 2012 :: HNil))
  }

}
