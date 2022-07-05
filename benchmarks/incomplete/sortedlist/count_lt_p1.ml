(** @synduce -s 2 -NB --no-lifting -n 20 --segis *)
(* Bug with se2gis on this one ... *)

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

let spec a l =
  let rec f = function
    | Elt x -> if x < a then 1 else 0
    | Cons (hd, tl) -> if hd < a then 1 + f tl else f tl
  in
  f l
;;

let target a l =
  let rec g = function
    | Elt x -> [%synt xi_0]
    | Cons (hd, tl) -> if hd < a then [%synt xi_1] else [%synt c0]
  in
  g l
  [@@requires is_sorted]
;;
