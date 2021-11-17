(** @synduce -NB --no-lifting *)

type tree =
  | Leaf of int
  | Node of int * int * tree * tree

let rec is_memo = function
  | Leaf x -> true
  | Node (parity, val_, l, r) ->
    is_parity parity l
    && is_parity parity r
    && val_ mod parity = 0
    && is_memo l
    && is_memo r

and is_parity y = function
  | Leaf x -> x mod y = 0
  | Node (parity, val_, l, r) -> val_ mod y = 0 && is_parity y l && is_parity y r
;;

let rec height = function
  | Leaf x -> true
  | Node (parity, val_, l, r) -> parity mod val_ = 0 && height l && height r
;;

(* A small variation of height. *)
let rec target = function
  | Leaf x -> [%synt f0] x
  | Node (parity, val_, l, r) -> [%synt join] val_
  [@@requires is_memo]
;;

assert (target = height)
