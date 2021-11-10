(** @synduce --no-lifting -NB *)

type 'a tree =
  | Nil
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec empty_right = function
  | Nil -> true
  | Leaf x -> true
  | Node (a, l, r) -> 0 = size r

and size = function
  | Nil -> 0
  | Leaf x -> 1
  | Node (a, l, r) -> 1 + size l + size r
  [@@ensures fun x -> x >= 0]
;;

let repr x = x

let rec spec = function
  | Nil -> 0
  | Leaf a -> a
  | Node (a, l, r) -> a + spec l + spec r
;;

let rec target = function
  | Nil -> [%synt xi_0]
  | Leaf a -> [%synt xi_1] a
  | Node (a, l, r) -> [%synt xi_2] a (target l)
  [@@requires empty_right]
;;
