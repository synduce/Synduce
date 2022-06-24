(** @synduce --no-lifting -NB -n 20 *)

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

let rec f = function
  | Leaf a -> if a < 10 then 1 else 0
  | Node (a, l, r) -> if a < 10 then 1 + f l + f r else f l + f r
  [@@ensures fun x -> x >= 0]
;;

let rec g = function
  | Leaf a -> [%synt fbase] a
  (* The important information here is the conditional, which is currently user-specified. *)
  | Node (a, l, r) ->
    if a > 10 then [%synt fthen] a (g r) else [%synt felse] a (g l) (g r)
  [@@requires is_bst]
;;

assert (g = f)
