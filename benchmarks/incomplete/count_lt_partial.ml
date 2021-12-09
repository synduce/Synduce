(** @synduce --no-lifting -NB -n 20 -s 2 -I bst.ml *)

open Bst

(* The specification is a binary search. *)
let spec x t =
  let rec f = function
    | Leaf a -> if a < x then 1 else 0
    | Node (a, l, r) -> if a < x then 1 + f l + f r else f l + f r
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

let target y t =
  let rec g l = [%synt g_body] in
  g t
  [@@requires is_bst]
;;
(*
Output of Synduce (1 solution found, but it's a naive solution)
----------------------------------------------------------------------------------------------------
 INFO : Total time spent in solvers:
        Z3-SMT     [   1 instances] 0.015s
        > TOTAL    [   1 instances]: 0.015s
 INFO : Solution found in 0.0194s (80.6% verifying):

let g_body02 a b c x = a > b ? (c + x) + 1 : c + x

let g_body03 y z = y > z ? 1 : 0

let rec g = function Leaf(l5) -> g_body03 x l5 | Node(l2, l3, l4) -> g_body02 x l2 (g l4) (g l3)


----------------------------------------------------------------------------------------------------
 INFO : Total time spent in solvers:
        Z3-SMT     [   3 instances] 0.018s
        CVC4-SMT   [   2 instances] 0.010s
        CVC-SyGuS  [   4 instances] 0.019s
        > TOTAL    [   9 instances]: 0.047s
 INFO : No solution: problem is unrealizable (found answer in 0.0232s).
 INFO : 💡 Explanation:
 INFO : On input Leaf(i0), g_body should have access to x .

----------------------------------------------------------------------------------------------------
 INFO : Total time spent in solvers:
        Z3-SMT     [   3 instances] 0.019s
        CVC4-SMT   [   2 instances] 0.010s
        CVC-SyGuS  [   2 instances] 0.019s
        > TOTAL    [   7 instances]: 0.048s
 INFO : No solution: problem is unrealizable (found answer in 0.0218s).
 INFO : 💡 Explanation:
 INFO : On input Leaf(i2), g_body0 should have access to i2 .

----------------------------------------------------------------------------------------------------
 INFO : Total time spent in solvers:
        Z3-SMT     [   6 instances] 0.073s
        CVC4-SMT   [  16 instances] 0.257s
        CVC-SyGuS  [  10 instances] 0.189s
        > TOTAL    [  32 instances]: 0.518s
 INFO : No solution: problem is unrealizable (found answer in 0.3620s).
 INFO : 💡 Explanation:
 INFO : On input Node(i3, p3, p4), g_body00 should have access to g p3
        It may help to add the recursive call having access to that value.

----------------------------------------------------------------------------------------------------
 INFO : Total time spent in solvers:
        Z3-SMT     [   9 instances] 0.242s
        CVC4-SMT   [  22 instances] 0.809s
        CVC-SyGuS  [  14 instances] 1.007s
        > TOTAL    [  45 instances]: 2.059s
 INFO : No solution: problem is unrealizable (found answer in 1.5268s).
 INFO : 💡 Explanation:
 INFO : On input Node(i497, p493, p494), g_body01 should have access to
        g p494 It may help to add the recursive call having access to that value.
./Synduce benchmarks/incomplete/count_lt_partial.ml  2.49s user 0.28s system 140% cpu 1.966 total

*)
