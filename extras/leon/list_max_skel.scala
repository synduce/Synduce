import leon.lang._
import leon.lang.synthesis._

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
        case CNil() => ???[Int]
        case Single(a) => ((i:Int) => ???[Int])(a)
        case Concat(x,y) => ((a:Int, b:Int)=> ???[Int])(f(x),f(y))
      }
    } ensuring (
      out => out == list_sum(repr(l1))
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
