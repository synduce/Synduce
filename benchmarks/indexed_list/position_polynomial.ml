(** @synduce --no-lifting *)

type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of int * list

type indexed_list =
  | CNil
  | CCons of int * int * indexed_list

let rec add_indices = function
  | Nil -> CNil
  | Cons (value, tl) -> CCons (value, length tl, add_indices tl)

and length = function
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl
;;

let rec polynome = function
  | CNil -> 0
  | CCons (hd, i, tl) -> (hd * i) + polynome tl
;;

let rec target = function
  | Nil -> [%synt s0]
  | Cons (hdv, tl) -> [%synt f] hdv (length tl) (target tl)

and length = function
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl
;;

assert (target = add_indices @@ polynome)
