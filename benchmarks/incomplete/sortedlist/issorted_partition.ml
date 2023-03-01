(** @synduce -s 2 -NB *)

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
  | Elt a -> true
  | Cons (hd, tl) ->
    let iss = spec tl in
    let next = head tl in
    iss && hd < next

and head = function
  | Elt a -> a
  | Cons (hd, tl) -> hd
;;

(** s0 and s1 should just be true here. If the inputs are "partitioned",
    they are effectively sorted.
 *)
let rec target = function
  | Single x -> [%synt s0]
  | Concat (hd, tl) -> [%synt s1]
  [@@requires is_partitioned]
;;

assert (target = clist_to_list @@ spec)
