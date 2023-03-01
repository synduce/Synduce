(* This benchmark is the same as benchmarks/tree/mips.ml but withtout the ensures clause. *)
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec repr x = x

let rec mips t = f (0, 0) t

and f s = function
  | Nil -> s
  | Node (a, l, r) ->
      let sum, m1 = f s l in
      f (sum + a, max (sum + a) m1) r

let rec hsum = function Nil -> [%synt s0] | Node (a, l, r) -> [%synt join] a (hsum l) (hsum r)

(* Declare the synthesis target *)
;;

assert (hsum = repr @@ mips)
