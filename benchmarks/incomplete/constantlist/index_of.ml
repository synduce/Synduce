(** @synduce -s 2 -NB *)

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

let rec constant_list = function
  | Elt x -> true
  | Cons (hd, tl) -> hd = head tl && constant_list tl

and head = function
  | Elt a -> a
  | Cons (hd, tl) -> hd
;;

let repr x = x

let spec x t =
  let rec f = function
    | Elt a -> if a = x then 1 else 0
    | Cons (hd, tl) -> if hd = x then 1 else if f tl = 0 then 0 else 1 + f tl
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

let target y t =
  let rec g = function
    | Elt a -> [%synt xi_0] y a
    | Cons (hd, tl) -> [%synt xi_1] y hd
  in
  g t
  [@@requires constant_list]
;;
