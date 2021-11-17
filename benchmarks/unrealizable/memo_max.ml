(** @synduce -NB --no-lifting *)

type tree =
  | Leaf of int
  | Node of int * int * int * tree * tree

let rec is_memo = function
  | Leaf x -> true
  | Node (lmax, rmax, val_, l, r) ->
    is_memo l
    && is_memo r
    && val_lt lmax l
    && val_lt rmax r
    && is_mem lmax l
    && is_mem rmax r

and val_lt hi = function
  | Leaf x -> x <= hi
  | Node (lmax, rmax, val_, l, r) -> val_ <= hi && rmax <= hi && lmax <= hi

and is_mem x = function
  | Leaf x -> x = x
  | Node (x, y, a, l, r) -> is_mem x l || is_mem x r || a = x
;;

let rec height = function
  | Leaf x -> x, x
  | Node (lmax, rmax, val_, l, r) ->
    let aminl, amaxl = height l in
    let aminr, amaxr = height r in
    min val_ (min aminl aminr), max val_ (max amaxl amaxr)
;;

(* A small variation of height. *)
let rec target = function
  | Leaf x -> [%synt f0] x
  | Node (lmax, rmax, val_, l, r) -> [%synt join] val_ lmax rmax
  [@@requires is_memo]
;;

assert (target = height)
