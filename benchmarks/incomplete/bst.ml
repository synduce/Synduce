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
