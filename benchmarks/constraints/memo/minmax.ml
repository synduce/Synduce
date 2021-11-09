(** @synduce -NBl --no-lifting *)

type tree =
  | Leaf of int
  | Node of int * int * int * int * int * tree * tree

let rec is_memo = function
  | Leaf x -> true
  | Node (lmin, lmax, rmin, rmax, val_, l, r) ->
    is_memo l && is_memo r && val_btw lmin lmax l && val_btw rmin rmax r

and val_btw lo hi = function
  | Leaf x -> lo = hi && hi = x
  | Node (lmin, lmax, rmin, rmax, val_, l, r) ->
    lo <= val_ && val_ <= hi && val_btw hi lo l && val_btw hi lo r
;;

let rec height = function
  | Leaf x -> x, x
  | Node (lmin, lmax, rmin, rmax, val_, l, r) ->
    let aminl, amaxl = height l in
    let aminr, amaxr = height r in
    min val_ (min aminl aminr), max val_ (max amaxl amaxr)
;;

(* A small variation of height. *)
let rec target = function
  | Leaf x -> [%synt f0] x
  | Node (lmin, lmax, rmin, rmax, val_, l, r) -> [%synt join] val_ lmin lmax rmin rmax
  [@@requires is_memo]
;;

assert (target = height)
