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
  | Nil -> true
  | Cons (x, l) -> if pr <= x then aux_up x l else aux_down x l

and aux_down pr = function
  | Nil -> true
  | Cons (x, l) -> pr >= x && aux_down x l
;;

(* This is just a sum to test the tool on accepting the unimodal list specification. *)
let spec x l =
  let rec f = function
    | Nil -> false
    | Cons (hd, tl) -> hd = x || f tl
  in
  f l
;;

let target x l =
  let rec g = function
    | UNil -> [%synt s0]
    | UElt a -> [%synt f0] x a
    | USplit (l, a, b, r) -> [%synt join] (x = a) (x = b) (g l) (g r)
  in
  g l
  [@@requires is_unimodal_ulist]
;;
