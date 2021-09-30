(** @synduce --no-lifting *)

type itree = Leaf of int | Node of int * itree * itree

let rec is_symmetric = function Leaf x -> true | Node (a, l, r) -> symm1 l r

and symm1 l = function Leaf x -> is_leaf x l | Node (ar, rl, rr) -> symm2 ar rl rr l

and symm2 ar rl rr = function
  | Leaf x -> false
  | Node (al, ll, lr) -> al = ar && symm1 lr rl && symm1 ll rr

and is_leaf x = function Leaf y -> x = y | Node (a, l, r) -> false

let rec minmax = function
  | Leaf x -> (x, x)
  | Node (a, l, r) ->
      let amin_l, amax_l = minmax l in
      let amin_r, amax_r = minmax r in
      (min amin_l (min amin_r a), max amin_l (max amax_r a))

(* In a symmetric tree we should only need to compute the sum for half the tree.  *)
let rec minmax2 = function Leaf x -> [%synt s0] x | Node (a, l, r) -> [%synt f0] a (minmax2 l)
  [@@requires is_symmetric]
;;

assert (minmax2 = minmax)
