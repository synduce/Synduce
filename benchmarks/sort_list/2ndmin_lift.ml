(** @synduce *)

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

let rec repr = function
  | Elt x -> Elt x
  | Cons (hd, tl) -> insert hd (repr tl)

and insert y = function
  | Elt x -> if y > x then Cons (y, Elt x) else Cons (x, Elt y)
  | Cons (hd, tl) -> if y > hd then Cons (y, Cons (hd, tl)) else Cons (hd, insert y tl)
;;

let rec spec = function
  | Elt x -> x
  | Cons (hd, tl) -> head tl

and head = function
  | Elt x -> x
  | Cons (hd, tl) -> hd
;;

let rec allpos = function
  | Elt x -> x > 0
  | Cons (hd, tl) -> hd > 0 && allpos tl && alldif hd tl

and alldif hd = function
  | Elt x -> not (hd = x)
  | Cons (x, tl) -> (not (hd = x)) && alldif hd tl
;;

let rec target = function
  | Elt x -> [%synt base_case] x
  | Cons (hd, tl) -> [%synt oplus] hd (target tl)
  [@@requires allpos]
;;
