(** @synduce -s 2 -NB -I ConsList.ml,ConcatList.ml *)

open ConsList
open ConcatList

(*
   This is our reference function. It is not named spec and we will need to
   declare our synthesis objective using an assert statement at the end of
   the file.
 *)
let rec mbo = function
  | Nil -> 0, 0, 0, true
  | Cons (hd, tl) ->
    let cl, ml, c, conj = mbo tl in
    let ncl = if hd then cl + 1 else 0 in
    let nconj = conj && hd in
    let nc = if nconj then c + 1 else c in
    ncl, max ml ncl, nc, nconj
  [@@ensures fun (cl, ml, c, conj) -> ml >= c && ml >= cl && c >= 0 && cl >= 0]
;;

(* This is the target function. There are three unknown components:
   s0, f0 and join
   *)
let rec hmbo = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0]
  | Concat (x, y) -> [%synt join]

(* The assertion should be of the form:
     assert (recursion skeleton = representation function @@ reference function)
 *)
;;

assert (hmbo = clist_to_conslist @@ mbo)
