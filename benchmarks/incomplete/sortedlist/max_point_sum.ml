(** @synduce -s 2 -NBc -n 30 --no-lifting *)

type list =
  | Elt of int * int
  | Cons of int * int * list

type clist =
  | Single of int * int
  | Concat of clist * clist

let rec repr = function
  | Single (a, b) -> Elt (a, b)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Single (a, b) -> Cons (a, b, repr l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let rec csorted l = sorted (repr l)

and sorted = function
  | Elt (x, y) -> true
  | Cons (x, y, l) -> aux (x + y) l

and aux a = function
  | Elt (x, y) -> a <= x + y
  | Cons (x, y, l) -> a <= x + y && aux (x + y) l
;;

let rec spec = function
  | Elt (a, b) -> a + b
  | Cons (a, b, tl) -> max (a + b) (spec tl)
;;

let rec target = function
  | Single (a, b) -> [%synt s0]
  | Concat (l, r) -> [%synt s1]
  [@@requires csorted]
;;

assert (target = repr @@ spec)
