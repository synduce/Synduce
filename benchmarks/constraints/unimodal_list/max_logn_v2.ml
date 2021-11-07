(** @synduce -NBk -n 30 *)

(* A data type representing lists in a tree format, where the list elements
   are kept where the list is "split" as opposed to concat-lists where
   all the elements are in the leaves.
*)
type ulist =
  | UElt of int
  | USplit of ulist * int * int * ulist

(* The usual type of cons-lists *)
type list =
  | Elt of int
  | Cons of int * list

(* Representation function from ulist -> list *)
let rec repr = function
  | UElt a -> Elt a
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, repr y))) x

and aux l = function
  | UElt a -> Cons (a, l)
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, aux l y))) x
;;

(* Invariant: unimodal list *)
let rec is_unimodal_ulist l = is_unimodal_list (repr l)

and is_unimodal_list = function
  | Elt x -> true
  | Cons (x, l) -> aux_up x l

and aux_up pr = function
  | Elt x -> pr < x
  | Cons (x, l) -> if pr < x then aux_up x l else aux_down x l

and aux_down pr = function
  | Elt x -> pr > x
  | Cons (x, l) -> pr > x && aux_down x l
;;

(* This is just a sum to test the tool on accepting the unimodal list specification. *)
let rec spec = function
  | Elt x -> x
  | Cons (hd, tl) -> max hd (spec tl)
;;

let rec target = function
  | UElt a -> [%synt f0] a
  | USplit (l, a, b, r) ->
    if a > b then [%synt joina] a (target l) else [%synt joinb] b (target r)
  [@@requires is_unimodal_ulist]
;;
