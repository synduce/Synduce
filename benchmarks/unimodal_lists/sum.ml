(* A data type representing lists in a tree format, where the list elements
  are kept where the list is "split" as opposed to concat-lists where
  all the elements are in the leaves.
 *)
type 'a ulist = UNil | UElt of 'a | USplit of 'a ulist * 'a * 'a * 'a ulist

(* The usual type of cons-lists *)
type 'a list = Nil | Cons of 'a * 'a list

(* Representation function from ulist -> list *)
let rec r = function
  | UNil -> Nil
  | UElt a -> Cons (a, Nil)
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, r y))) x

and aux l = function
  | UNil -> l
  | UElt a -> Cons (a, l)
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, aux l y))) x

(* The sum function on lists. *)
let rec sum = function Nil -> 0 | Cons (hd, tl) -> hd + sum tl

(*
  Testing a not-so-natural specification.
  Here we test first whether  at a node, the labels are zero, and if that
  is the case use a function that only takes as input the two recursive calls.
  This makes the synthesis times much longer.
*)
let rec g = function
  | UNil -> [%synt s0]
  | UElt a -> [%synt f0] a
  | USplit (x, a, b, y) -> if a + b = 0 then [%synt f1] (g x) (g y) else [%synt f2] a b (g x) (g y)

;;
assert (g = r @@ sum)
