(** @synduce --no-lifting -s 2  -I ConsList.ml,ConcatList.ml *)

(* Using ConsList and ConcatList Synduce cannot use open directives, so the path to the source file
  of the module has to be used, like in the comment above.
  The path is relative to the file being synthesized.
 *)
open ConsList
open ConcatList

let rec sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl
;;

let rec hsum = function
  | l -> [%synt sum_body]
;;

assert (hsum = clist_to_conslist @@ sum)
