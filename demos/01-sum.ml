type list =
  | Nil (* The empty list. *)
  | Cons of int * list (* Construct a list by adding an element at the front. *)

type concat_list =
  (* The empty list. *)
  | CNil
  (* A singleton list. *)
  | Single of int
  (* Construct a list by concatenating two lists. *)
  | Concat of concat_list * concat_list

(*
  This is the representation function.
  Ignore it for now!
 *)
let rec clist_to_conslist = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_conslist l1
  | Single a -> Cons (a, clist_to_conslist l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

(*
  This is our reference function: it sums
  the elements of a cons-list.
*)
let rec sum = function
  | Nil -> 0
  | Cons (head, tail) -> head + sum tail
;;

(* This is the target function.
  There are three unknown components:
  u0, u1 and u2
  *)
let rec hsum = function
  | CNil -> [%synt u0]
  | Single a -> [%synt u1] a
  | Concat (x, y) -> [%synt u2] (hsum x)
;;

assert (hsum = clist_to_conslist @@ sum)
