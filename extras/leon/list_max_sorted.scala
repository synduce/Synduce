import leon.lang._
import leon.lang.synthesis._

object ListOperations {
    sealed abstract class List
    case class Cons(head: Int, tail: List) extends List
    case class Elt(a : Int) extends List

    sealed abstract class CList
    case class Concat(left : CList, right : CList) extends CList
    case class Single(elt : Int) extends CList

	def content(l: List) : Set[Int] = l match {
	    case Elt(a) => Set(a)
	    case Cons(head, tail) => Set(head) ++ content(tail)
	}

    def max(a : Int, b : Int) : Int = {
      if (a > b) {a} else {b}
    }

    def sorted(l : List) : Boolean = l match {
        case Elt(a) => true
        case Cons(head, tail) => sorted_aux(head, tail)
    }

    def sorted_aux(prev : Int, l : List) : Boolean = l match {
        case Elt(a) => prev > a
        case Cons(head, tail) =>  prev > head && sorted_aux(head, tail)
    }


    def list_max(l : List) : Int = l match {
      case Elt(a) => a
      case Cons(head, tail) => max(head, list_max(tail))
    }

    def repr(l : CList) : List = l match {
      case Single(a) => Elt(a)
      case Concat(x,y) => repr_aux(y,x)
    }

    def repr_aux(l : CList, y : CList) : List = y match {
      case Single(a) => Cons(a, repr(l))
      case Concat(y1, y2) => repr_aux(Concat(l, y2), y1)
    }


    def f(l1 : List) : Int = {
      l1 match {
        case Elt(a) => ???[Int]
        case Cons(x,y) => ???[Int]
      }
    } ensuring (
      out => (! sorted(l1) || (out == list_max(l1)))
    )
}
// pmrs repr l =
//     repr l -> c l
//     | c CNil -> Nil
//     | c Single(a) -> Cons(a, Nil)
//     | c Concat(x, y) -> dec y x
//     | dec l CNil -> repr l
//     | dec l Single(a) -> Cons(a, repr l)
//     | dec l Concat(x, y) -> dec (Concat(l, y)) x
