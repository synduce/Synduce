(** @synduce -s 2 -NB *)

type list =
  | Elt of int
  | Cons of int * list

(* Concat-list of nested list with pivot *)
type cnlist =
  | Sglt of int
  | Cat of cnlist * int * cnlist

let rec clist_to_list = function
  | Sglt a -> Elt a
  | Cat (x, piv, y) -> dec y x

and dec l1 = function
  | Sglt a -> Cons (a, clist_to_list l1)
  | Cat (x, piv, y) -> dec (Cat (y, piv, l1)) x
;;

(* Type invariant: partitioned by sum value *)
let rec sorted = function
  | Sglt a -> true
  | Cat (x, piv, y) -> lmax x < piv && piv < lmin y

and lmin = function
  | Sglt a -> a
  | Cat (x, piv, y) -> min (lmin x) (lmin y)

and lmax = function
  | Sglt a -> a
  | Cat (x, piv, y) -> max (lmax x) (lmax y)
;;

(* Reference function : sum of longest suffis of positive elements. *)
let rec spec = function
  | Elt a -> max 0 a, a >= 0
  | Cons (hd, tl) ->
    let mtss, cond = spec tl in
    let new_cond = hd >= 0 && cond in
    (if new_cond then mtss + hd else mtss), new_cond
;;

let rec target = function
  | Sglt x -> [%synt s0] x
  | Cat (l, piv, r) -> [%synt f2] piv (target r) (target l)
;;

assert (target = clist_to_list @@ spec)
