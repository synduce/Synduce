(** @synduce -NB -n 10 --no-gropt *)

type tree =
  | Leaf of int * int
  | Node of int * int * tree * tree

let rec tree_min = function
  | Leaf (x, y) -> x
  | Node (a, b, l, r) -> min a (min (tree_min l) (tree_min r))
;;

let rec tree_max = function
  | Leaf (x, y) -> x
  | Node (a, b, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let rec is_bst = function
  | Leaf (a, b) -> a < b
  | Node (a, b, l, r) -> a < b && a > tree_max l && a < tree_min r && is_bst l && is_bst r
;;

let repr x = x

let spec hi lo t =
  let rec f = function
    | Leaf (a, b) -> lo < a && b < hi
    | Node (a, b, l, r) -> (lo < a && b < hi) || f l || f r
  in
  f t
;;

let target hi lo t =
  let rec g = function
    | Leaf (a, b) -> [%synt xi_0] hi lo a b
    | Node (a, b, l, r) ->
      if b > hi
      then if a > lo then [%synt xi_11] (g r) (g l) else [%synt xi_12] (g r)
      else [%synt xi_2] hi lo b a (g l) (g r)
  in
  g t
  [@@requires is_bst]
;;
