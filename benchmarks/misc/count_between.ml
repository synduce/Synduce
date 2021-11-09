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

let repr x = x

let spec lo hi t =
  let rec f = function
    | Leaf a -> lo < a && a < hi
    | Node (a, l, r) -> (lo < a && a < hi) || f l || f r
  in
  f t
;;

let target lo hi t =
  let rec g = function
    | Leaf a -> [%synt s0] (lo, hi) a
    | Node (a, l, r) ->
      if a >= hi then [%synt gt_case] (g l) (g r) else [%synt le_case] a lo (g l) (g r)
  in
  g t
;;
