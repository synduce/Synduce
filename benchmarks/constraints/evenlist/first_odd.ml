(** @synduce -NB -n 20 *)

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

let rec even_positive_list = function
  | Elt x -> x > 0 && x mod 2 = 0
  | Cons (hd, tl) -> hd > 0 && hd mod 2 = 0 && even_positive_list tl
;;

let rec spec = function
  | Elt x -> if x mod 2 = 1 then x else 0
  | Cons (a, l) -> if a mod 2 = 1 then a else spec l
;;

let rec target = function
  | Elt x -> [%synt s0]
  | Cons (a, l) -> [%synt f0]
  [@@requires even_positive_list]
;;
