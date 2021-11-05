(** @synduce --no-gropt *)

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

(* Representation function: sort a list. *)
let rec repr = function
  | Elt x -> Elt x
  | Cons (hd, tl) -> insert hd (repr tl)

and insert y = function
  | Elt x -> if y < x then Cons (y, Elt x) else Cons (x, Elt y)
  | Cons (hd, tl) -> if y < hd then Cons (y, Cons (hd, tl)) else Cons (hd, insert y tl)
;;

(* Invairant: length >= 2 *)
let rec is_length_lt2 l = len l >= 2

and len = function
  | Elt x -> 1
  | Cons (hd, tl) -> 1 + len tl
;;

let rec spec = function
  | Elt x -> 1
  | Cons (hd, tl) -> 1 + spec tl
;;

let rec target = function
  | Elt x -> 1
  | Cons (hd, tl) -> [%synt join] (target tl)
  [@@requires is_length_lt2]
;;
