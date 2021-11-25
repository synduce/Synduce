(** @synduce -I ConsList.ml,ConcatList.ml *)

(* Using ConsList and ConcatList Synduce cannot use open directives, so the path to the source file
  of the module has to be used, like in the comment above.
  The path is relative to the file being synthesized.
 *)
open ConsList
open ConcatList

(*
  This is our reference function. It is not named spec and we will need to
  declare our synthesis objective using an assert statement at the end of
  the file.
*)
let rec sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl
;;

(* This is the target function. There are three unknown components:
  s0, f0 and join
  *)
let rec hsum = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt join] (hsum x) (hsum y)

(* The assertion should be of the form:
    assert (recursion skeleton = representation function @@ reference function)
*)
;;

assert (hsum = clist_to_conslist @@ sum)
