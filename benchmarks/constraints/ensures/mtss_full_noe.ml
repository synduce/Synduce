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

let rec spec = function
  | Line a -> max 0 (bsum a), bsum a
  | NCons (hd, tl) ->
    let mtss, csum = spec tl in
    let line_sum = bsum hd in
    max (mtss + line_sum) 0, csum + line_sum

and bsum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + bsum tl
;;

let rec target = function
  | Sglt x -> [%synt s0] (inner x)
  | Cat (l, r) -> [%synt s1] (target r) (target l)

and inner = function
  | Elt x -> [%synt inner0] x
  | Cons (hd, tl) -> [%synt inner1] hd (inner tl)
;;

assert (target = clist_to_list @@ spec)
