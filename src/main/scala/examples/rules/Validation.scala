package examples.rules

//https://github.com/davegurnell/functional-data-validation
object Validation {

  sealed trait Result[+A] {
    def ap[B](fn: Result[A => B]): Result[B] =
      (this, fn) match {
        case (Pass(a), Pass(b)) => Pass(b(a))
        case (Fail(a), Pass(b)) => Fail(a)
        case (Pass(a), Fail(b)) => Fail(b)
        case (Fail(a), Fail(b)) => Fail(a ++ b)
      }

    def &[B, C](that: Result[B])(func: (A, B) => C): Result[C] =
      ap(that.map((b: B) => (a: A) => func(a, b)))

    def map[B](func: A => B) = this match {
      case Pass(a) => Pass(func(a))
      case Fail(a) => Fail(a)
    }

    def flatMap[B](func: A => Result[B]) = this match {
      case Pass(a) => func(a)
      case Fail(a) => Fail(a)
    }
  }

  final case class Pass[A](value: A) extends Result[A]
  final case class Fail(messages: List[String]) extends Result[Nothing]

  type Rule[-A, +B] = A => Result[B]

  implicit class RuleOps[A, B](rule: Rule[A, B]) {
    def map[C](func: B => C): Rule[A, C] =
      (a: A) => rule(a) map func

    def flatMap[C](rule2: Rule[B, C]): Rule[A, C] =
      (a: A) => rule(a) flatMap rule2

    def and[C, D](rule2: Rule[A, C])(func: (B, C) => D): Rule[A, D] =
      (a: A) => (rule(a) & rule2(a))(func)
  }

  def validator[A]: Rule[A, A] = (input: A) => Pass(input)

  def main(args: Array[String]) = {
    case class Event(number: Int, source: String, sink: String)

    val nonEmpty: Rule[String, String] = (str: String) => if(str.isEmpty) Fail(List("Empty string")) else Pass(str)

    def capitalize(str: String): String = str(0).toUpper +: str.substring(1)

    def gte(min: Int) = (num: Int) => if(num < min) Fail(List("Too small number")) else Pass(num)

    val checkNumber: Rule[Event, Int] =
      validator[Event] map (_.number) flatMap gte(124)

    val checkStreet: Rule[Event, String] =
      validator[Event] map (_.source) flatMap nonEmpty map capitalize

    val checkZip: Rule[Event, String] =
      validator[Event] map (_.sink) flatMap nonEmpty

    val checkAddress: Rule[Event, Event] =
      (address: Event) =>
        checkZip(address).ap {
          checkStreet(address).ap {
            checkNumber(address).ap {
              Pass {
                (number: Int) =>
                  (street: String) =>
                    (zipCode: String) =>
                      Event(number, street, zipCode)
              }
            }
          }
        }

    /*
    val read: Rule[Event, Event] =
      (address: Event) =>
        //could be read from somewhere
        Pass(address)

    read(Event(123, "Twitter", "Cassandra")).flatMap(checkAddress)
    */

    println(checkAddress(Event(123, "NewYork", "")))
  }
}