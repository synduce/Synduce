(** @synduce --no-lifting -NB -n 30 *)

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

type 'a clist =
  | Single of 'a
  | Concat of 'a clist * 'a clist

let rec clist_to_list = function
  | Single a -> Elt a
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

(** Predicate asserting that a concat-list is partitioned.  *)
let rec is_partitioned = function
  | Single x -> true
  | Concat (x, y) -> lmax x < lmin y && is_partitioned x && is_partitioned y

and lmax = function
  | Single x -> x
  | Concat (x, y) -> max (lmax x) (lmax y)

and lmin = function
  | Single x -> x
  | Concat (x, y) -> min (lmin x) (lmin y)
;;

let rec spec = function
  | Elt a -> a
  | Cons (hd, tl) -> max hd (spec tl)
;;

let rec target = function
  | Single x -> [%synt s0] x
  | Concat (l, r) -> [%synt s1] (target r)
  [@@requires is_partitioned]
;;

assert (target = clist_to_list @@ spec)
