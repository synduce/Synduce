(* A data type representing lists in a tree format, where the list elements
   are kept where the list is "split" as opposed to concat-lists where
   all the elements are in the leaves.
*)
type 'a ulist =
  | UNil
  | UElt of 'a
  | USplit of 'a ulist * 'a * 'a * 'a ulist

(* The usual type of cons-lists *)
type 'a list =
  | Nil
  | Cons of 'a * 'a list

(* Representation function from ulist -> list *)
let rec r = function
  | UNil -> Nil
  | UElt a -> Cons (a, Nil)
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, r y))) x

and aux l = function
  | UNil -> l
  | UElt a -> Cons (a, l)
  | USplit (x, a, b, y) -> aux (Cons (a, Cons (b, aux l y))) x
;;

(* Invariant: unimodal list *)
let rec is_unimodal_ulist l = is_unimodal_list (r l)

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
let rec f = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + f tl
;;

let rec g = function
  | UNil -> [%synt s0]
  | UElt a -> [%synt f0] a
  | USplit (x, a, b, y) -> [%synt join] a b (g x) (g y)
  [@@requires is_unimodal_ulist]
;;

assert (g = r @@ f)
