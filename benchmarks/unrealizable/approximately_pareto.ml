(** @synduce -NB -n 20 --no-lifting *)

type 'a tree =
  | Leaf of 'a * 'a
  | Node of 'a * 'a tree * 'a tree

let rec tree_min = function
  | Leaf (x, y) -> x + y
  | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))
;;

let rec tree_max = function
  | Leaf (x, y) -> x + y
  | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let rec is_bst = function
  | Leaf (x, y) -> true
  | Node (a, l, r) -> a >= tree_max l && a <= tree_min r && is_bst l && is_bst r
;;

let repr x = x

let rec spec = function
  | Leaf (x, y) -> x, y
  | Node (a, l, r) ->
    let x1, y1 = spec l in
    let x2, y2 = spec r in
    if x1 >= x2 || y1 >= y2 then x1, y1 else x2, y2
;;

let rec target = function
  | Leaf (x, y) -> [%synt xi_0] y x
  | Node (a, l, r) ->
    if a >= 0 then [%synt xi_1] (target l) else [%synt xi_2] a (target r)
  [@@requires is_bst]
;;
