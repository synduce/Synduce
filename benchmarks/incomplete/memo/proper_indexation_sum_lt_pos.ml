(** @synduce -s 2 -NB *)

type list =
  | Nil
  | Cons of int * list

type indexed_list =
  | CNil
  | CCons of int * int * indexed_list

let rec is_proper_indexed = function
  | CNil -> true
  | CCons (value, index, tl) -> index = length tl && is_proper_indexed tl

and length = function
  | CNil -> 0
  | CCons (_, _, tl) -> 1 + length tl
;;

let rec project = function
  | CNil -> Nil
  | CCons (value, _, tl) -> Cons (value, project tl)
;;

let rec polynome = function
  | Nil -> 0
  | Cons (hd, tl) -> if hd > length tl then hd + polynome tl else polynome tl

and length = function
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl
;;

let rec target = function
  | CNil -> [%synt s0]
  | CCons (hdv, hdi, tl) -> [%synt f] hdv hdi (target tl)
  [@@requires is_proper_indexed]
;;

assert (target = project @@ polynome)
