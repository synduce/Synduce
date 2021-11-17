(** @synduce -NB *)

type clist =
  | Empty
  | Elt of int
  | Concat of clist * clist

type list =
  | Nil
  | Cons of int * list

(* Invariant *)
let rec allpos = function
  | Empty -> true
  | Elt x -> x > 0
  | Concat (x, y) -> allpos x && allpos y
;;

let rec spec = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let amin, sec_min = spec tl in
    min hd amin, min sec_min (max hd amin)
  [@@ensures fun (x, y) -> x <= y]
;;

let rec target = function
  | Empty -> [%synt init]
  | Elt a -> [%synt base_case] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)
  [@@requires allpos]
;;

let rec repr = function
  | Empty -> Nil
  | Elt x -> Cons (x, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | Empty -> repr l
  | Elt a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;
