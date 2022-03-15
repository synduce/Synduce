(** @synduce -NB -n 30 --no-lifting *)

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
  | Node (a, l, r) -> a >= tree_max l && a <= tree_min r && is_bst l && is_bst r
;;

let repr x = x

let spec x t =
  let rec f = function
    | Leaf a -> if a < x && a mod 2 = 0 then 1 else 0
    | Node (a, l, r) -> if a < x && a mod 2 = 0 then 1 + f l + f r else f l + f r
  in
  f t
;;

let target y t =
  let rec g = function
    | Leaf a -> [%synt xi_0] y a
    | Node (a, l, r) -> if a < y then [%synt xi_1] a y (g l) (g r) else [%synt xi_2] (g l)
  in
  g t
  [@@requires is_bst]
;;
