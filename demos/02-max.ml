type concat_list =
  (* Base case: list of length > 1 *)
  | Elt of int
  | Cons of int * concat_list

(* A predicate indicating the list
   is sorted.
*)
let rec is_sorted = function
  | Elt x -> true
  | Cons (head, tail) -> aux head tail

and aux prev = function
  | Elt x -> prev >= x
  | Cons (hd, tl) -> prev >= hd && aux hd tl
;;

let rec maxs = function
  | Elt x -> x
  | Cons (head, tail) -> max head (maxs tail)
;;

let rec amax = function
  | Elt x -> [%synt base] x
  | Cons (hd, _) -> [%synt join] hd
  [@@requires is_sorted]
;;

assert (amax = maxs)
