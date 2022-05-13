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
