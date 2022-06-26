(** @synduce -s 2 -NB *)

type list =
  | Elt of int
  | Cons of int * list

let rec is_sorted = function
  | Elt x -> true
  | Cons (hd, tl) -> hd <= head tl && is_sorted tl

and head = function
  | Elt x -> x
  | Cons (hd, tl) -> hd
;;

let rec spec = function
  | Elt x -> x
  | Cons (hd, tl) -> min hd (spec tl)
;;

let rec target = function
  | Elt x -> [%synt xi_0] x
  | Cons (hd, tl) -> [%synt xi_1] hd
  [@@requires is_sorted]
;;
