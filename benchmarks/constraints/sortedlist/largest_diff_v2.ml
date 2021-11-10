type list =
  | Elt of int
  | Cons of int * list

type clist =
  | Single of int
  | Concat of clist * clist

(* First write the is_sorted function. *)
let rec is_sorted = function
  | Elt x -> true
  | Cons (hd, tl) -> aux hd tl

and aux prev = function
  | Elt x -> prev >= x
  | Cons (hd, tl) -> prev >= hd && aux hd tl
;;

(* This is the representation function *)
let rec repr = function
  | Single a -> Elt a
  | Concat (x, y) -> dec y x

and dec l = function
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;

(* Use synduce to synthesize is_sorted but on Concat lists! (see benchmarks/list/is_sorted.ml) *)

let rec is_concat_sorted l = (fun (x, y, z) -> z) (is_concat_sorted_aux l)

and is_concat_sorted_aux = function
  | Single a -> a, a, true
  | Concat (y, z) ->
    (fun (a, b, c) (a2, b2, c2) -> a, b2, (a2 > b && c) && c2)
      (is_concat_sorted_aux y)
      (is_concat_sorted_aux z)
;;

(* Reference function. *)
let rec ldiff = function
  | Elt x -> x, x, 0
  | Cons (hd, tl) ->
    let amin, amax, r = ldiff tl in
    let amin = min amin hd in
    let amax = max amax hd in
    amin, amax, max (amax - amin) r
;;

(* Synthesize a linear function. *)
let rec amax = function
  | Single x -> [%synt g0] x
  | Concat (x, y) -> [%synt join] (amax x) (amax y)
  [@@requires is_concat_sorted]
;;

assert (amax = repr @@ ldiff)
