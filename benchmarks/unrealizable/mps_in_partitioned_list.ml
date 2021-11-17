(** @synduce --no-lifting -NB -n 30 *)

type list =
  | Elt of int
  | Cons of int * list

type nested_list =
  | Line of list
  | NCons of list * nested_list

(* Concat-list of nested list with pivot *)
type cnlist =
  | Sglt of list
  | Cat of cnlist * int * cnlist

let rec clist_to_list = function
  | Sglt a -> Line a
  | Cat (x, piv, y) -> dec y x

and dec l1 = function
  | Sglt a -> NCons (a, clist_to_list l1)
  | Cat (x, piv, y) -> dec (Cat (y, piv, l1)) x
;;

(* Type invariant: partitioned by sum value *)
let rec sorted = function
  | Sglt a -> true
  | Cat (x, piv, y) -> lmax x <= piv && piv <= lmin y

and lmin = function
  | Sglt a -> lsum a
  | Cat (x, piv, y) -> min (lmin x) (lmin y)

and lmax = function
  | Sglt a -> lsum a
  | Cat (x, piv, y) -> min (lmin x) (lmin y)

and lsum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + lsum tl
;;

(* Reference function *)
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
  | Cat (l, piv, r) ->
    if piv <= 0 then [%synt f1] (target r) else [%synt f2] (target r) (target l)
  [@@requires sorted]

and inner = function
  | Elt x -> [%synt inner0] x
  | Cons (hd, tl) -> [%synt inner1] hd (inner tl)
;;

assert (target = clist_to_list @@ spec)
