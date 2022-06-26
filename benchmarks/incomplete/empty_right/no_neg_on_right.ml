(** @synduce -s 2 -NB  *)

type node =
  | Pos
  | Neg

type tree =
  | Leaf of node
  | Node of node * tree * tree

let rec no_neg_right = function
  | Leaf x -> true
  | Node (a, l, r) -> has_no_neg r

and has_no_neg = function
  | Leaf x -> is_not_neg x
  | Node (a, l, r) -> is_not_neg a && has_no_neg l && has_no_neg r

and is_not_neg = function
  | Neg -> false
  | Pos -> true
;;

let repr x = x

let rec spec = function
  | Leaf a -> if is_neg a then 1 else 0
  | Node (a, l, r) ->
    if size r >= 0 && is_neg a then 1 + spec l + spec r else spec l + spec r
  [@@ensures fun x -> x >= 0]

and size = function
  | Leaf x -> 1
  | Node (a, l, r) -> 1 + size l + size r

and is_neg = function
  | Pos -> false
  | Neg -> true
;;

let rec target = function
  | Leaf a -> [%synt xi_1] a
  | Node (a, l, r) -> [%synt xi_2] a (target l)
  [@@requires no_neg_right]
;;
