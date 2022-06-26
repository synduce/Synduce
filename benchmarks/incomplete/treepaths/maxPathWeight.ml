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

let rec mpath = function
  | Empty -> 0
  | Node (a, l, r) -> a + max (mpath l) (mpath r)
;;

let rec main = function
  | Top -> 0
  | Zip (x, a, child, z) -> [%synt join]

and recall = function
  | Empty -> 0
  | Node (a, l, r) -> a + max (recall l) (recall r)

and color = function
  | Left -> true
  | Right -> false
;;

let rec repr = function
  | Top -> Empty
  | Zip (w, lbl, child, z) -> h lbl child z w

and h lbl child z = function
  | Left -> Node (lbl, child, repr z)
  | Right -> Node (lbl, repr z, child)
;;

assert (main = repr @ mpath)
