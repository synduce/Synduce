(** @synduce -s 2 -NB --no-lifting *)

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

let search x l =
  let rec aux = function
    | CNil -> 0
    | CCons (hd, i, tl) -> if x = hd then i else aux tl
  in
  aux l
;;

let target x l =
  let rec bux = function
    | Nil -> [%synt s0]
    | Cons (hdv, tl) -> [%synt f]
  and length = function
    | Nil -> 0
    | Cons (_, tl) -> 1 + length tl
  in
  bux l
;;

assert (target = add_indices @@ search)
