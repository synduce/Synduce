(** @synduce --no-lifting -NB -n 20 -s 2 -I bst.ml *)

open Bst

let rec f = function
  | Leaf a -> if a < 10 then 1 else 0
  | Node (a, l, r) -> if a < 10 then 1 + f l + f r else f l + f r
  [@@ensures fun x -> x >= 0]
;;

let rec g = function
  | Leaf a -> [%synt fbase]
  (* The important information here is the conditional, which is currently user-specified. *)
  | Node (a, l, r) -> if a > 10 then [%synt fthen] else [%synt felse]
  [@@requires is_bst]
;;

assert (g = f)
