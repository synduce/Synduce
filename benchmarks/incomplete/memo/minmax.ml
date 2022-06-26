(** @synduce -s 2 -NB --no-lifting -n 20 *)

type tree =
  | Leaf of int
  | Node of int * int * int * int * int * tree * tree

let rec is_memo = function
  | Leaf x -> true
  | Node (lmin, lmax, rmin, rmax, val_, l, r) ->
    is_memo l
    && is_memo r
    && lmin = amin l
    && lmax = amax l
    && rmin = amin r
    && rmax = amax r

and amin = function
  | Leaf x -> x
  | Node (x, y, z, w, a, l, r) -> min a (min (amin l) (amin r))

and amax = function
  | Leaf x -> x
  | Node (x, y, z, w, a, l, r) -> max a (max (amax l) (amax r))
;;

let rec minmax = function
  | Leaf x -> x, x
  | Node (lmin, lmax, rmin, rmax, val_, l, r) ->
    let aminl, amaxl = minmax l in
    let aminr, amaxr = minmax r in
    min val_ (min aminl aminr), max val_ (max amaxl amaxr)
  [@@ensures fun (x, y) -> x <= y]
;;

(* A small variation of height. *)
let rec target = function
  | Leaf x -> [%synt f0] x
  | Node (lmin, lmax, rmin, rmax, val_, l, r) -> [%synt join] val_ lmin lmax rmin rmax
  [@@requires is_memo]
;;

assert (target = minmax)
