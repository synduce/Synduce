(** @synduce -s 2 -NB *)

type btree =
  | Empty
  | Node of int * btree * btree

type sel =
  | Left
  | Right

type zipper =
  | Top
  | Zip of sel * int * btree * zipper

let rec height = function
  | Empty -> 0
  | Node (a, l, r) -> 1 + max (height l) (height r)
;;

let rec target = function
  | Top -> 0
  | Zip (x, a, child, z) -> sel_deconstr a child z x

and sel_deconstr a child z = function
  | Left -> [%synt joinl]
  | Right -> [%synt joinr]

(* Copy of height here so it can be used in synthesis. *)
and h = function
  | Empty -> 0
  | Node (a, l, r) -> 1 + max (h l) (h r)
;;

let rec repr = function
  | Top -> Empty
  | Zip (w, lbl, child, z) -> h lbl child z w

and h lbl child z = function
  | Left -> Node (lbl, child, repr z)
  | Right -> Node (lbl, repr z, child)
;;

assert (target = repr @ height)
