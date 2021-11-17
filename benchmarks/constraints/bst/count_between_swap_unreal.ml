(** @synduce -NB -n 30 *)
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

let spec hi lo t =
  let rec f = function
    | Leaf a -> if hi > a && a > lo then 1 else 0
    | Node (a, l, r) -> if hi > a && a > lo then 1 + f l + f r else f l + f r
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

(* This problem is unrealizable,
  and illustrates how Synduce can "help" by quickly answering UNSAT.
*)
let target hi lo t =
  let rec g = function
    | Leaf a -> [%synt xi_0] hi lo a
    | Node (a, l, r) ->
      if a < lo
      then [%synt f_a_lt_lo] (g l)
      else if a > hi
      then [%synt f_a_gt_hi] (g r)
      else [%synt f_else] a hi lo (g r) (g l)
  in
  g t
  [@@requires is_bst]
;;
