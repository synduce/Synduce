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
  | Line a -> bmts a
  | NCons (hd, tl) ->
    let hi, lo = spec tl in
    let line_lo, line_hi = bmts hd in
    min line_lo lo, max line_hi hi

and bmts = function
  | Elt x -> x, max x 0
  | Cons (hd, tl) ->
    let asum, amts = bmts tl in
    asum + hd, max (amts + hd) 0
;;

let rec target = function
  | Sglt x -> inner x
  | Cat (l, r) -> [%synt s1] (target r) (target l)

and inner = function
  | Elt x -> [%synt inner0] x
  | Cons (hd, tl) -> [%synt inner1] hd (inner tl)
;;

assert (target = clist_to_list @@ spec)
