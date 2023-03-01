(** @synduce -s 2 --no-lifting -NB -n 20  -I bst.ml *)

open Bst

let spec x t =
  let rec f = function
    | Leaf a -> if a < x then 1 else 0
    | Node (a, l, r) -> if a < x then 1 + f l + f r else f l + f r
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

let target y t =
  let rec g = function
    | Leaf a -> [%synt fbase] y a
    (* The important information here is the conditional, which is currently user-specified. *)
    | Node (a, l, r) -> if a < y then [%synt fthen] (g l) (g r) else [%synt felse] (g l)
  in
  g t
  [@@requires is_bst]
;;
