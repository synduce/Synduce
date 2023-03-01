(** @synduce -s 2 -NB *)

(* The type of labelled binary trees *)
type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

(* Zipper for labelled binary trees *)
type sel =
  | Left
  | Right

type 'c zipper =
  | Top
  | Zip of sel * 'c * 'c btree * 'c zipper

let rec spec = function
  | Empty -> false, 1
  | Node (a, l, r) ->
    let b1, x1 = spec l in
    if b1 then b1, x1 else if a mod 2 = 1 then true, a else spec r
;;

let rec target = function
  | Top -> [%synt s0]
  | Zip (c, a, child, z) -> aux a child z c

and aux a child z = function
  | Left -> [%synt joinl]
  | Right -> [%synt joinr]

and og = function
  | Empty -> false, 1
  | Node (a, l, r) ->
    let b1, x1 = og l in
    if b1 then b1, x1 else if a mod 2 = 1 then true, a else og r
;;

let rec repr = function
  | Top -> Empty
  | Zip (w, lbl, child, z) -> h lbl child z w

and h lbl child z = function
  | Left -> Node (lbl, child, repr z)
  | Right -> Node (lbl, repr z, child)
;;
