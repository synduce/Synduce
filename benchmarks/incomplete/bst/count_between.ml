(** @synduce -A --no-lifting -n 20 -I bst.ml *)

(* This skeleton has erros and is unrealizable, but allowing for different configuations
  allows to find solutions that are variations of the configuration given here.
  This skeleton has 262144 possible configurations.
*)

open Bst

let spec hi lo t =
  let rec f = function
    | Leaf a -> if hi > a && a > lo then 1 else 0
    | Node (a, l, r) -> if hi > a && a > lo then 1 + f l + f r else f l + f r
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

let target hi lo t =
  let rec g = function
    | Leaf a -> [%synt xi_0] hi lo a
    | Node (a, l, r) ->
      if a < lo
      then [%synt f_a_lt_lo] (g l)
      else if a > hi
      then [%synt f_a_gt_hi] (g r)
      else [%synt f_else] a hi lo (g r) (g l)
  in
  g t
  [@@requires is_bst]
;;
