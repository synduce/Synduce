(** @synduce -s 2 -NB *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

(*
  This is our reference function. It is not named spec and we will need to
  declare our synthesis objective using an assert statement at the end of
  the file.
*)
let rec sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl
;;

(* This is the representation function. Remark that it is the same as the one
  defined in the mpshom.pmrs or sumhom.pmrs files!
 *)
let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

(* This is the target function. There are three unknown components:
  s0, f0 and join
  *)
let rec hsum = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> 0 + [%synt f1] + [%synt f2]

(* The assertion should be of the form:
    assert (recursion skeleton = representation function @@ reference function)
*)
;;

assert (hsum = clist_to_list @@ sum)
