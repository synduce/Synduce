(** @synduce *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec spec = function
  | Leaf a -> a > 0
  | Node (a, l, r) -> a > top l && a > top r && spec l && spec r

and top = function
  | Node (a, l, r) -> a
  | Leaf a -> a
;;

let rec target = function
  | Leaf a -> [%synt s0] a
  | Node (a, l, r) -> [%synt j0] a (target l) (target r)
;;

assert (target = spec)
