type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec spec t = f 0 t [@@ensures fun x -> x >= 0]

and f s = function
  | Nil -> s
  | Node (a, l, r) ->
      let _mts = f s l in
      let _mts' = max (_mts + a) 0 in
      f _mts' r

let rec mits = function Nil -> [%synt s0] | Node (a, l, r) -> [%synt join1] a (mits l) (mits r)

let repr x = x

(* This should fail in no time. Solver returned unsat in less than a second. *)

;;
assert (mits = repr @@ spec)
