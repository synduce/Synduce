(** @synduce -NB *)

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

let rec even_list = function
  | Elt x -> is_even x
  | Cons (hd, tl) -> is_even hd && even_list tl

and is_even x = x mod 2 = 0

let rec spec = function
  | Elt x -> x mod 2
  | Cons (a, l) -> a mod 2
  [@@ensures fun x -> x >= 0]
;;

let rec target = function
  | Elt x -> [%synt s0]
  | Cons (a, l) -> [%synt f0] (target l)
  [@@requires even_list]
;;
