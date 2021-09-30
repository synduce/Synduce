(** @synduce --no-lifting *)

type itree = Null | Node of int * itree * itree

let rec is_symmetric = function Null -> true | Node (a, l, r) -> symm1 l r

and symm1 l = function Null -> is_null l | Node (ar, rl, rr) -> symm2 ar rl rr l

and symm2 ar rl rr = function
  | Null -> false
  | Node (al, ll, lr) -> al = ar && symm1 lr rl && symm1 ll rr

and is_null = function Null -> true | Node (a, l, r) -> false

let rec sum = function Null -> 0 | Node (a, l, r) -> a + sum l + sum r

(* In a symmetric tree we should only need to compute the sum for half the tree.  *)
let rec sum2 = function Null -> [%synt s0] | Node (a, l, r) -> [%synt f0] a (sum2 l)
  [@@requires is_symmetric]
;;

assert (sum2 = sum)
