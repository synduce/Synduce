(** @synduce --no-lifting -NB -n 30 *)

type list =
  | Elt of int
  | Cons of int * list

type nested_list =
  | Line of list
  | NCons of list * nested_list

type cnlist =
  | Sglt of list
  | Cat of cnlist * cnlist

let rec clist_to_list = function
  | Sglt a -> Line a
  | Cat (x, y) -> dec y x

and dec l1 = function
  | Sglt a -> NCons (a, clist_to_list l1)
  | Cat (x, y) -> dec (Cat (y, l1)) x
;;

(** Predicate asserting that a concat-list is partitioned.  *)
let rec is_partitioned = function
  | Sglt x -> true
  | Cat (x, y) -> nlmax x < nlmin y && is_partitioned x && is_partitioned y

and nlmax = function
  | Sglt x -> sum x
  | Cat (x, y) -> max (nlmax x) (nlmax y)

and nlmin = function
  | Sglt x -> sum x
  | Cat (x, y) -> min (nlmin x) (nlmin y)

and sum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + sum tl
;;

let rec spec = function
  | Line a -> max 0 (bsum a), bsum a
  | NCons (hd, tl) ->
    let mbs, csum = spec tl in
    let line_sum = bsum hd in
    max (line_sum + mbs) 0, csum + line_sum

and bsum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + bsum tl
;;

let rec target = function
  | Sglt x -> [%synt s0] (tsum x)
  | Cat (l, r) -> [%synt s1] (target r) (target l)

and tsum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + tsum tl
;;

assert (target = clist_to_list @@ spec)
