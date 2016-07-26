package examples.prolog

/**
  http://rnduja.github.io/2016/01/19/a_shapeless_primer/

  import examples.prolog.FamilyTree._

  GrandChild[John, Tom] works
  GrandChild.path[John, Carl, Tom] works
  GrandChild[Tom, John] error

  Edge.from[Carl] works
  Edge.from[John] error

  Edge.to[Tom]  works
  Edge.to[Carl] works
  Edge.to[John] error

  */
object FamilyTree {

  trait Node
  type N = Node
  trait John extends Node
  trait Carl extends Node
  trait Tom extends Node

  trait Child[T <: N, U <: N]
  trait GrandChild[T <: N, U <: N]

  //Facts database
  implicit val john_carl = new Child[John, Carl] {}
  implicit val carl_tom = new Child[Carl, Tom] {}

  /**
    Rule: If there is a child relationship between X and Y and between Y and Z
    when X and Z are in grandChild relationship
    */
  object GrandChild {

    /**
      Having X and Y please find evidence that in the facts db exist 2 facts
      such that GrandChild[X, Y] and Child[Y, Z]
      */
    def apply[X <: N, Z <: N](implicit x: Child[X, _], y: Child[_, Z]) =
      new GrandChild[X, Z] {}

    def path[X <: N, Y <: N, Z <: N](implicit x: Child[X, Y], y: Child[Y, Z]) =
      new GrandChild[X, Z] {}
  }

  object Edge {

    /**
      Edge.from[Carl] works
      Edge.from[John] error
      */
    def from[X <: N](implicit x: Child[X, _]) =
      new GrandChild[X, Y forSome { type Y <: N }] {}

    /**
      Edge.to[Tom]  works
      Edge.to[Carl] works
      Edge.to[John] error
      */
    def to[X <: N](implicit x: Child[_, X]) =
      new GrandChild[Y forSome { type Y <: N }, X] {}
  }

  //implicit def grandChild[X, Y, Z](implicit xy: Child[X, Y], yz: Child[Y, Z]) = new GrandChild[X, Z] {}
}
