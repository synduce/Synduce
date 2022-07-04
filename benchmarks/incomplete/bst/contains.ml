(** @synduce -A 30. -NB -n 20 *)

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
    | Leaf a -> if a = x then 1 else 0
    | Node (a, l, r) ->
      if a = x then 1 else if f l = 1 then 1 else if f r = 1 then 1 else 0
  in
  f t
  [@@ensures fun x -> x >= 0 && x <= 1]
;;

let target y t =
  let rec g = function
    | Leaf a -> [%synt xi_0] y a
    | Node (a, l, r) -> if y < a then [%synt xi_1] (g l) else [%synt xi_2] y a (g r)
  in
  g t
  [@@requires is_bst]
;;
