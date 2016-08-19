package examples

//https://gist.github.com/milessabin/25b7b669b5a9ac051a71
package object enums {

  object Values {
    implicit def conv[T](self: this.type)(implicit v: MkValues[T]): Set[T] = Values[T]

    def apply[T](implicit v: MkValues[T]): Set[T] = v.values.toSet

    trait MkValues[T] {
      def values: List[T]
    }

    object MkValues {
      import shapeless._

      implicit def values[T, Repr <: Coproduct](implicit gen: Generic.Aux[T, Repr], v: Aux[T, Repr]): MkValues[T] =
          new MkValues[T] { def values = v.values }

      trait Aux[T, Repr] {
        def values: List[T]
      }

      object Aux {
        implicit def cnilAux[A]: Aux[A, CNil] =
          new Aux[A, CNil] { def values = Nil }

        implicit def cconsAux[T, L <: T, R <: Coproduct]
          (implicit l: Witness.Aux[L], r: Aux[T, R]): Aux[T, L :+: R] =
          new Aux[T, L :+: R] { def values = l.value :: r.values }
      }
    }
  }

  sealed trait WeekDay
  object WeekDay {
    case object Mon extends WeekDay
    case object Tue extends WeekDay
    case object Wed extends WeekDay
    case object Thu extends WeekDay
    case object Fri extends WeekDay
    case object Sat extends WeekDay
    case object Sun extends WeekDay

    val values: Set[WeekDay] = Values
  }


  import WeekDay._

  def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)

  def isWeekend(d: WeekDay) = d match {
    case Sat | Sun => true
    case _ => false // compile time non-exhaustive match warning/error without this case
  }

  assert((WeekDay.values filter isWorkingDay) == Set(Mon, Tue, Wed, Thu, Fri))
  assert(!isWeekend(Mon)) //
}