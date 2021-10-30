(** @synduce --no-lifting *)

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

type 'a clist =
  | Single of 'a
  | Concat of 'a list * 'a list

let rec repr = function
  | Single a -> Elt a
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Elt x -> Cons (x, l1)
  | Cons (x, y) -> Cons (x, dec l1 y)
;;

(** Predicate asserting that a concat-list is partitioned.  *)
let rec is_partitioned = function
  | Single x -> true
  | Concat (x, y) -> lmin x > lmax y

and lmax = function
  | Elt x -> x
  | Cons (x, y) -> max x (lmax y)

and lmin = function
  | Elt x -> x
  | Cons (x, y) -> min x (lmin y)
;;

let rec spec = function
  | Elt a -> a
  | Cons (hd, tl) -> max hd (spec tl)
;;

let rec target = function
  | Single x -> [%synt s0] x
  | Concat (l, r) -> [%synt s1] (spec l)
  [@@requires is_partitioned]
;;
