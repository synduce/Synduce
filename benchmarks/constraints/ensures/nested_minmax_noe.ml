(** @synduce --no-lifting -NB -n 50 *)

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
  | Line a -> brange a
  | NCons (hd, tl) ->
    let lo, hi = spec tl in
    let line_lo, line_hi = brange hd in
    min line_lo lo, max line_hi hi

and brange = function
  | Elt x -> x, x
  | Cons (hd, tl) ->
    let lo, hi = brange tl in
    min lo hd, max hi hd
;;

let rec target = function
  | Sglt x -> inner x
  | Cat (l, r) -> [%synt s1] (target r) (target l)

and inner = function
  | Elt x -> [%synt inner0] x
  | Cons (hd, tl) -> [%synt inner1] hd (inner tl)
;;

assert (target = clist_to_list @@ spec)
