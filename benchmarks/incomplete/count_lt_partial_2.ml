(** @synduce --no-lifting -NB -n 20 -s 2 -I bst.ml *)

open Bst

(* The reference function is a function that counts all the elements
  less than some parameter in the tree.
  [spec x t] counts all the elements less than x in the binary tree t.
 *)
let spec x t =
  let rec f = function
    | Leaf a -> if a < x then 1 else 0
    | Node (a, l, r) -> if a < x then 1 + f l + f r else f l + f r
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

(*
  The target recursion skeleton is under-specified. All the unknowns have no arguments
  and it is the synthesizer's task to figure out what to give as arguments to the unknowns.
  [target y t] should return the number of elements less than [y] in [t] when [t] is a binary
  search tree.
*)
let target y t =
  let rec g = function
    | Leaf a -> [%synt fbase]
    (* The important information here is the conditional, which is currently user-specified. *)
    | Node (a, l, r) -> if a < y then [%synt fthen] else [%synt felse]
  in
  g t
  [@@requires is_bst]
;;

(*
Synduce tries 10 different recursion skeletons in 73s and finds the following 2 solutions:
The "best" solution is this one:
let fbase0 a b = a > b ? 1 : 0
let felse7 c = c
let fthen2 x y = (x + y) + 1
let rec g =
  function Leaf(a) -> fbase0 x a | Node(a, l, r) -> a < x ? fthen2 (g r) (g l) : felse7 (g l)

And a solution that does not use the bst-property of the tree:
let fbase0 z w = z > w ? 1 : 0
let felse8 u v = u + v
let fthen2 a b1 = (a + b1) + 1
let rec g =
  function Leaf(a) -> fbase0 x a | Node(a, l, r) -> a < x ? fthen2 (g r) (g l) : felse8 (g r) (g l)

Most of the time is spent in proving unrealizability of unrealiazable skeletons. The realizable ones
takes 1s and 40 ms respectively.

*)
