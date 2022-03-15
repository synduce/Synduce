(** @synduce -NB -n 30 --no-lifting *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

type 'a counttree =
  | CLeaf of 'a
  | CNode of 'a * int * 'a counttree * 'a counttree

let rec tree_min = function
  | CLeaf x -> x
  | CNode (a, _, l, r) -> min a (min (tree_min l) (tree_min r))
;;

let rec tree_max = function
  | CLeaf x -> x
  | CNode (a, _, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let rec count_elts = function
  | CLeaf _ -> 1
  | CNode (_, _, l, r) -> count_elts l + count_elts r + 1
;;

let rec is_bst = function
  | CLeaf x -> true
  | CNode (a, c, l, r) ->
    c = count_elts l + count_elts r + 1
    && a >= tree_max l
    && a <= tree_min r
    && is_bst l
    && is_bst r
;;

let rec repr = function
  | CLeaf x -> Leaf x
  | CNode (a, c, l, r) -> Node (a, repr l, repr r)
;;

let spec x t =
  let rec f = function
    | Leaf a -> (if a < x && a mod 2 = 0 then 1 else 0), 1
    | Node (a, l, r) ->
      let el1, el2 = f l in
      let er1, er2 = f r in
      (if a < x && a mod 2 = 0 then 1 + el1 + er1 else el1 + er1), el2 + er2 + 1
  in
  f t
;;

let target y t =
  let rec g = function
    | CLeaf a -> [%synt xi_0] y a
    | CNode (a, c, l, r) ->
      if a < y then [%synt xi_1] c a y (g l) (g r) else [%synt xi_2] c (g l)
  in
  g t
  [@@requires is_bst]
;;
