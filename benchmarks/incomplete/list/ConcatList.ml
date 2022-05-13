(** This module contains definitions  for the Concat-List type, with variants
    depending on the base case.
*)

open ConsList

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a eclist =
  | ESingle of 'a
  | ECat of 'a eclist * 'a eclist

(* This is the representation function. Remark that it is the same as the one
  defined in the mpshom.pmrs or sumhom.pmrs files!
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

(* This is the representation function. Remark that it is the same as the one
  defined in the mpshom.pmrs or sumhom.pmrs files!
 *)
let rec eclist_to_eltlist = function
  | ESingle a -> Elt a
  | ECat (x, y) -> dec y x

and dec l1 = function
  | ESingle a -> ECons (a, eclist_to_eltlist l1)
  | ECat (x, y) -> dec (ECat (y, l1)) x
;;
