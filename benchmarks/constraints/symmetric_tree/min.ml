(** @synduce -s 2 --no-lifting -NB --se2gis *)

type itree =
  | Leaf of int
  | Node of int * itree * itree

let rec is_symmetric = function
  | Leaf x -> true
  | Node (a, l, r) -> symm1 l r && is_symmetric l && is_symmetric r

and symm1 l = function
  | Leaf x -> is_leaf x l
  | Node (ar, rl, rr) -> symm2 ar rl rr l

and symm2 ar rl rr = function
  | Leaf x -> false
  | Node (al, ll, lr) -> al = ar && symm1 lr rl && symm1 ll rr

and is_leaf x = function
  | Leaf y -> x = y
  | Node (a, l, r) -> false
;;

let rec amin = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (amin l) (amin r))
;;

(* In a symmetric tree we should only need to compute the min for half the tree.  *)
let rec amin2 = function
  | Leaf x -> [%synt s0] x
  | Node (a, l, r) -> [%synt f0] a (amin2 l)
  [@@requires is_symmetric]
;;

assert (amin2 = amin)
