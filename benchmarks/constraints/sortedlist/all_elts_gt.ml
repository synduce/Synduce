(** @synduce -NB -n 30 -W cvc4  *)

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
  | Node (a, l, r) -> a > tree_max l && a < tree_min r && is_bst l && is_bst r
;;

let repr x = x

module ISet = Set.Make (Int)

let rec spec = function
  | Leaf a -> if a > 0 then ISet.singleton a else ISet.empty
  | Node (a, l, r) ->
    if a > 0
    then ISet.add a (ISet.union (spec l) (spec r))
    else ISet.union (spec l) (spec r)
;;

let rec target = function
  | Leaf a -> [%synt xi_0] a
  | Node (a, l, r) ->
    if a < 0 then [%synt xi_1] (target r) else [%synt xi_2] a (target r) (target l)
  [@@requires is_bst]
;;
