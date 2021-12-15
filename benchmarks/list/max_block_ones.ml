(** @synduce -I ConsList.ml,ConcatList.ml *)

(* #include "decl_header.h"

int _max_length_of_1 (_Bool *a, int n) {
  int cl = 0;
  int ml = 0;
  int c = 0;
  _Bool conj = true;

  for (int i = 0; i < n; i++) {
    cl = a[i] ? cl + 1 : 0;
    ml = max (ml, cl);
    // Auxliiary variables
    conj = conj && a[i];
    c = c + (conj ? 1 : 0);
  }
  return ml + c;
}

/*
  WIth auxliary variables and starting from the functional representation,
  the tool finds the following join :
  conj = conj-l && conj-r
  cl = (! conj-$R) ? cl-r (cl-l + c-r)
  c =  c-l + (! conj-$L) ? 0 : c-r
  ml = max ((+ cl-l c-r), (max (ml-r ml-l))
*/ *)

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
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt join] (hmbo x) (hmbo y)

(* The assertion should be of the form:
     assert (recursion skeleton = representation function @@ reference function)
 *)
;;

assert (hmbo = clist_to_conslist @@ mbo)
