(* A different type for lists: lists are split and the elements at the split are
  arguments of the constructor.
 *)
type 'a splitlist = SNil | SElt of 'a | Split of 'a splitlist * 'a * 'a * 'a splitlist

type 'a list = Nil | Cons of 'a * 'a list

let rec sum = function Nil -> 0 | Cons (hd, tl) -> hd + sum tl

let rec clist_to_list = function
  | SNil -> Nil
  | SElt a -> Cons (a, Nil)
  | Split (x, a, b, y) -> dec (Cons (a, Cons (b, clist_to_list y))) x

and dec l1 = function
  | SNil -> l1
  | SElt a -> Cons (a, l1)
  | Split (x, a, b, y) -> dec (Cons (a, Cons (b, dec l1 y))) x

(* Target function: synthesize hsum s.t. hsum(x) = sum(clist_to_list(x)) *)
let rec hsum = function
  | SNil -> [%synt s0]
  | SElt a -> [%synt f0] a
  | Split (x, a, b, y) -> [%synt join] a b (hsum x) (hsum y)

;;
assert (hsum = clist_to_list @@ sum)
