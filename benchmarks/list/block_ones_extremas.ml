(** @synduce -I ConsList.ml,ConcatList.ml *)

(* Using ConsList and ConcatList Synduce cannot use open directives, so the path to the source file
  of the module has to be used, like in the comment above.
  The path is relative to the file being synthesized.
 *)
open ConsList
open ConcatList

let rec mbo = function
  | Nil -> 0, 0, true
  | Cons (hd, tl) ->
    let cl, c, conj = mbo tl in
    let ncl = if hd then cl + 1 else 0 in
    let nconj = conj && hd in
    let nc = if nconj then c + 1 else c in
    ncl, nc, nconj
;;

let rec hmbo = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt join] (hmbo x) (hmbo y)

(* The assertion should be of the form:
     assert (recursion skeleton = representation function @@ reference function)
 *)
;;

assert (hmbo = clist_to_conslist @@ mbo)
