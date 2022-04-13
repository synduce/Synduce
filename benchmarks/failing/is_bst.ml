type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec spec = function
  | Leaf x -> true
  | Node (a, l, r) -> a > tmax l && a < tmin r && spec l && spec r

and tmax = function
  | Leaf x -> x
  | Node (a, l, r) -> max a (max (tmax l) (tmax r))

and tmin = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (tmin l) (tmin r))
;;

(*
  We can formulate the problem of synthesizing a more efficient check that a tree is a binary tree.
  However, the problem is that we cannot reason about min_int and max_int ...
*)
let rec target t = f (Int.min_int, Int.max_int) t

and f bounds = function
  | Leaf x -> [%synt op] x bounds (* x > lo && x < hi *)
  | Node (a, l, r) ->
    [%synt op2] a bounds (f ([%synt fl] bounds a) l) (f ([%synt fr] bounds a) r)
;;
(* a > lo && a < hi && f (lo, a) l && f (a, hi) r *)
