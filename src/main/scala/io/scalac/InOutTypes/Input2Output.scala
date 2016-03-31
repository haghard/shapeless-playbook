package io.scalac.InOutTypes

//http://stackoverflow.com/questions/36310264/how-to-define-a-function-whose-output-type-depends-on-the-input-type/36322221
object Input2Output {

  case class AddRequest(x: Int, y: Int)

  case class ToUppercaseRequest(str: String)

  case class AddResponse(sum: Int)

  case class ToUppercaseResponse(upper: String)

  sealed trait Flow[In] {
    type Out

    def apply(in: In): Out
  }

  object Flow {
    type Aux[I, O] = Flow[I] {type Out = O}

    implicit def toUppercase: Aux[ToUppercaseRequest, ToUppercaseResponse] =
      new Flow[ToUppercaseRequest] {
        type Out = ToUppercaseResponse

        def apply(in: ToUppercaseRequest): ToUppercaseResponse =
          ToUppercaseResponse(in.str.toUpperCase)
      }

    implicit def add: Aux[AddRequest, AddResponse] =
      new Flow[AddRequest] {
        type Out = AddResponse

        def apply(in: AddRequest): AddResponse = AddResponse(in.x + in.y)
      }

    def apply[In](in: In)(implicit e: Flow[In]) = e(in)
  }

  /** ************************************************************************/
  sealed trait Flow2[Out] {
    def computed: Out
  }

  case class AddRequest1(x: Int, y: Int) extends Flow2[AddResponse] {
    def computed = AddResponse(x + y)
  }

  case class ToUppercaseRequest1(str: String) extends Flow2[ToUppercaseResponse] {
    def computed = ToUppercaseResponse(str.toUpperCase)
  }

  def process2[O](in: Flow2[O]): O = in.computed

  /** ************************************************************************/
  trait Request[T <: Response]

  trait Response

  case class SumReq(x: Int, y: Int) extends Request[SumResp]

  case class SumResp(sum: Int) extends Response

  sealed trait Flow3[In, Out] {
    def exec(in: In): Out
  }

  object Flow3 {
    def apply[In <: Request[Out], Out <: Response](implicit e: Flow3[In, Out]) = e

    implicit def exchange = new Flow3[SumReq, SumResp] {
      override def exec(in: SumReq) = SumResp(in.x + in.y)
    }
  }

  /** *************************************************************************/

  def main(args: Array[String]) = {
    val sum1 = process2(AddRequest1(1, 2))
    val line1 = process2(ToUppercaseRequest1("qwerty"))

    val sum2 = (Flow3[SumReq, SumResp] exec SumReq(1, 2))

    val sum = Flow(AddRequest(1, 2))
    val line = Flow(ToUppercaseRequest("qwerty"))

    println(sum)
    println(line)

    println(sum1)
    println(line1)

    println(sum2)
  }
}