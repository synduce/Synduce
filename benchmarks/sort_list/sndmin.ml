type list =
  | Nil
  | Cons of int * list

type list2 =
  | Nil2
  | Cons2 of int * int * list2

(* Representation function: sort the list then build a list2 *)
let rec repr l = dcomp (sort l)

and dcomp = function
  | Nil -> Nil2
  | Cons (hd, tl) -> dcomp2 hd tl

and dcomp2 x = function
  | Nil -> Cons2 (x, 0, Nil2)
  | Cons (hd, tl) -> Cons2 (x, hd, dcomp tl)

and sort = function
  | Nil -> Nil
  | Cons (hd, tl) -> insert hd (sort tl)

and insert y = function
  | Nil -> Cons (y, Nil)
  | Cons (hd, tl) -> if y < hd then Cons (y, Cons (hd, tl)) else Cons (hd, insert y tl)
;;

let rec spec = function
  | Nil2 -> 0
  | Cons2 (hd1, hd2, tl) -> hd2
;;

let rec allneg = function
  | Nil -> true
  | Cons (hd, tl) -> hd < 0 && allneg tl && alldif hd tl

and alldif hd = function
  | Nil -> true
  | Cons (x, tl) -> (not (hd = x)) && alldif hd tl
;;

let rec target = function
  | Nil -> [%synt base_case]
  | Cons (hd, tl) -> [%synt oplus] hd (target tl)
  [@@requires allneg]
;;
