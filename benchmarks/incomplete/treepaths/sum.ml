(** @synduce -s 2 -NB *)

type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

type sel =
  | Left
  | Right

type 'c zipper =
  | Top
  | Zip of sel * 'c * 'c btree * 'c zipper

let rec sum = function
  | Empty -> 0
  | Node (a, l, r) -> a + sum l + sum r
;;

let rec main = function
  | Top -> [%synt s0]
  | Zip (x, a, child, z) -> [%synt join]

and aux = function
  | Empty -> 0
  | Node (a, l, r) -> a + sum l + sum r
;;

let rec repr = function
  | Top -> Empty
  | Zip (w, lbl, child, z) -> h lbl child z w

and h lbl child z = function
  | Left -> Node (lbl, child, repr z)
  | Right -> Node (lbl, repr z, child)
;;

assert (main = repr @ sum)
