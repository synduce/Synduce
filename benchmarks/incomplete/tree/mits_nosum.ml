(** @synduce --cvc4 *)

(* cvc4 is much faster on this benchmark. *)

type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let rec spec t = f 0 t [@@ensures fun x -> x >= 0]

and f s = function
  | Nil -> s
  | Node (a, l, r) ->
    let mts_l = f s l in
    let mts_r = max (mts_l + a) 0 in
    f mts_r r
;;

let rec mits = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt join1] a (mits l) (mits r)
;;

let repr x = x

(* This should fail in no time. Solver returned unsat in less than a second. *)
;;

assert (mits = repr @@ spec)
