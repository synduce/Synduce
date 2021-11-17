(** @synduce -lkNB -n 20 *)

(* A data type representing lists in a tree format, where the list elements
   are kept where the list is "split" as opposed to concat-lists where
   all the elements are in the leaves.
*)
type ulist =
  | UNil
  | UElt of int
  | USplit of ulist * int * int * ulist

(* The usual type of cons-lists *)
type list =
  | Nil
  | Cons of int * list

(* Representation function from ulist -> list *)
let rec repr = function
  | UNil -> Nil
  | UElt a -> Cons (a, Nil)
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, repr y))) x

and aux l = function
  | UNil -> l
  | UElt a -> Cons (a, l)
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, aux l y))) x
;;

(* Invariant: unimodal list *)
let rec is_unimodal_ulist l = is_unimodal_list (repr l)

and is_unimodal_list = function
  | Nil -> true
  | Cons (x, l) -> aux_up x l

and aux_up pr = function
  | Nil -> pr > 0
  | Cons (x, l) -> pr > 0 && if pr < x then aux_up x l else aux_down x l

and aux_down pr = function
  | Nil -> pr > 0
  | Cons (x, l) -> pr > 0 && pr > x && aux_down x l
;;

(* This is just a sum to test the tool on accepting the unimodal list specification. *)
let rec spec = function
  | Nil -> 0
  | Cons (hd, tl) -> max hd (spec tl)
  [@@ensures fun x -> x >= 0]
;;

let rec target = function
  | UNil -> [%synt s0]
  | UElt a -> [%synt f0] a
  | USplit (l, a, b, r) ->
    if a > b then [%synt joina] a b (target l) else [%synt joinb] a b (target r)
  [@@requires is_unimodal_ulist]
;;
