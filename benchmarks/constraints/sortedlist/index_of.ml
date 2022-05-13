(** @synduce -NB -n 20 *)

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

let spec x t =
  let rec f = function
    | Elt a -> if a = x then 1 else 0
    | Cons (hd, tl) -> if hd = x then 1 else if f tl = 0 then 0 else 1 + f tl
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

let target x t =
  let rec g = function
    | Elt a -> [%synt xi_0] x a
    | Cons (hd, tl) -> if hd >= x then [%synt xi_1] x hd else [%synt xi_2] x hd (g tl)
  in
  g t
  [@@requires is_sorted]
;;
