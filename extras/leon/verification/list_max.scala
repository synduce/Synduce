import leon.lang._

object ListOperations {
    sealed abstract class List
    case class Cons(head: Int, tail: List) extends List
    case class Nil() extends List

    sealed abstract class CList
    case class Concat(left : CList, right : CList) extends CList
    case class Single(elt : Int) extends CList
    case class CNil() extends CList

	  def content(l: List) : Set[Int] = l match {
	    case Nil() => Set.empty
	    case Cons(head, tail) => Set(head) ++ content(tail)
	  }

    def list_sum(l : List) : Int = l match {
      case Nil() => 0
      case Cons(head, tail) => head + list_sum(tail)
    }

    def repr(l : CList) : List = l match {
      case CNil() => Nil()
      case Single(a) => Cons(a, Nil())
      case Concat(x,y) => repr_aux(y,x)
    }

    def repr_aux(l : CList, y : CList) : List = y match {
      case CNil() => repr(l)
      case Single(a) => Cons(a, repr(l))
      case Concat(y1, y2) => repr_aux(Concat(l, y2), y1)
    }


    def f(l1 : CList) : Int = {
      l1 match {
        case CNil() => 0
        case Single(a) => a
        case Concat(x,y) => f(x) + f(y)
      }
    }

    def equivalence(l1 : CList) = {
        f(l1) == list_sum(repr(l1))
    } holds
}

// Output of Leon on this verification problem:
// [Warning ] Refusing to inline recursive function 'listWebElementToWebTree'!
// [  Info  ]  - Now considering 'match exhaustiveness' VC for content @13:45...
// [  Info  ]  => VALID
// [  Info  ]  - Now considering 'match exhaustiveness' VC for list_sum @18:36...
// [  Info  ]  => VALID
// [  Info  ]  - Now considering 'match exhaustiveness' VC for repr @23:34...
// [  Info  ]  => VALID
// [  Info  ]  - Now considering 'match exhaustiveness' VC for repr_aux @29:49...
// [  Info  ]  => VALID
// [  Info  ]  - Now considering 'match exhaustiveness' VC for f @37:7...
// [  Info  ]  => VALID
// [  Info  ]  - Now considering 'postcondition' VC for equivalence @45:9...
//
// .. and then Leon hangs.
