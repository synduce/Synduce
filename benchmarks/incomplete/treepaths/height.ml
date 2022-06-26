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

let rec height = function
  | Empty -> 0
  | Node (a, l, r) -> 1 + max (height l) (height r)
;;

let rec target = function
  | Top -> 0
  | Zip (x, a, child, z) -> sel_deconstr a child z x

and sel_deconstr a child z = function
  | Left -> [%synt joinl] a (height child) (target z)
  | Right -> [%synt joinr] a (height child) (target z)
;;

let rec repr = function
  | Top -> Empty
  | Zip (w, lbl, child, z) -> h lbl child z w

and h lbl child z = function
  | Left -> Node (lbl, child, repr z)
  | Right -> Node (lbl, repr z, child)
;;

assert (target = repr @ height)
