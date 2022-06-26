(** @synduce -s 2 -NB *)

type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let rec repr x = x

let rec mips t = f (0, 0) t [@@ensures fun (x, y) -> y >= 0]

and f s = function
  | Nil -> s
  | Node (a, l, r) ->
    let sum, m1 = f s l in
    f (sum + a, max (sum + a) m1) r
;;

let rec hsum = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt join]

(* Declare the synthesis target *)
;;

assert (hsum = repr @@ mips)
