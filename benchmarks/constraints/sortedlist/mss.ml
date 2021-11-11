(** @synduce --no-lifting -NB -n 30 *)

type list =
  | Elt of int
  | Cons of int * list

type clist =
  | Single of int
  | Concat of int * clist * clist

let rec repr = function
  | Single a -> Elt a
  | Concat (a, x, y) -> dec y x

and dec l1 = function
  | Single a -> Cons (a, repr l1)
  | Concat (a, x, y) -> dec (Concat (a, y, l1)) x
;;

(** Predicate asserting that a concat-list is partitioned.  *)
let rec is_partitioned = function
  | Single x -> true
  | Concat (a, x, y) -> lmax x < a && a < lmin y && is_partitioned x && is_partitioned y

and lmax = function
  | Single x -> x
  | Concat (a, x, y) -> max (lmax x) (lmax y)

and lmin = function
  | Single x -> x
  | Concat (a, x, y) -> min (lmin x) (lmin y)
;;

let rec f = function
  | Elt a -> a, max a 0, max a 0, max a 0
  | Cons (hd, tl) ->
    let sum, mts, mps, mss = f tl in
    sum + hd, max mts (sum + hd), max (mps + hd) 0, max mss (max (mps + hd) 0)
  [@@ensures
    fun (sum, mts, mps, mss) ->
      mts >= 0
      && mps >= 0
      && mps >= sum
      && mts >= sum
      && mss >= 0
      && mss >= mts
      && mss >= sum
      && mss >= mps]
;;

let rec h = function
  | Single a -> [%synt f0] a
  | Concat (a, y, z) ->
    if a < 0 then [%synt f1] (asum y) (h z) else [%synt odot] (h y) (h z)
  [@@requires is_partitioned]

and asum = function
  | Single a -> a
  | Concat (a, y, z) -> asum y + asum z
;;

assert (h = repr @@ f)
