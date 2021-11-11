(** @synduce -NB -n 30 --no-lifting *)

type tree =
  | Leaf of int
  | Node of int * tree * tree

type mtree =
  | MLeaf of int
  | MNode of int * int * mtree * mtree

let rec repr = function
  | MLeaf i -> Leaf i
  | MNode (a, s, l, r) -> Node (a, repr l, repr r)
;;

let rec tree_min = function
  | MLeaf x -> x
  | MNode (a, s, l, r) -> min a (min (tree_min l) (tree_min r))
;;

let rec tree_max = function
  | MLeaf x -> x
  | MNode (a, s, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let rec is_bst = function
  | MLeaf a -> a > 0
  | MNode (a, s, l, r) ->
    s = tsum l + tsum r
    && a > 0
    && a > tree_max l
    && a < tree_min r
    && is_bst l
    && is_bst r

and tsum = function
  | MLeaf a -> a
  | MNode (a, s, l, r) -> a + tsum l + tsum r
;;

let repr x = x

let rec spec = function
  | Leaf a -> a < 4, a
  | Node (a, l, r) ->
    let okl, sl = spec l in
    let okr, sr = spec r in
    okl && okr && sl + sr + a <= 2, sl + sr + a
;;

let rec target = function
  | MLeaf a -> [%synt xi_0] a
  | MNode (a, s, l, r) ->
    if a > 2
    then [%synt c0], [%synt xi_1] s a
    else [%synt xi_3] s a (target l) (target r), [%synt xi_2] s
  [@@requires is_bst]
;;
