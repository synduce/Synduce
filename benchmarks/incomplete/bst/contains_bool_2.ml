(** @synduce -s 2 -NB --no-lifting *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec tree_min = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))
;;

let rec tree_max = function
  | Leaf x -> x
  | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let rec is_bst = function
  | Leaf x -> true
  | Node (a, l, r) -> a >= tree_max r && a <= tree_min l && is_bst l && is_bst r
;;

let repr x = x

let spec x t =
  let rec f = function
    | Leaf a -> a = x
    | Node (a, l, r) -> a = x || f l || f r
  in
  f t
;;

let target y t =
  let rec g = function
    | Leaf a -> [%synt xi_0] y a
    | Node (a, l, r) -> if y < a then [%synt xi_1] (g r) else [%synt xi_2] y a (g l)
  in
  g t
  [@@requires is_bst]
;;
