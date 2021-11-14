(** @synduce *)

type 'a clist =
  | Elt of 'a
  | Cons of 'a * 'a clist

let rec is_sorted = function
  | Elt x -> true
  | Cons (hd, tl) -> aux hd tl

and aux prev = function
  | Elt x -> prev >= x
  | Cons (hd, tl) -> prev >= hd && aux hd tl
;;

let rec lpen = function
  | Elt x -> if x mod 2 = 0 then x else 0
  | Cons (hd, tl) ->
    let r = lpen tl in
    if hd mod 2 = 0 && hd > 0 then max hd r else r
;;

let rec amax = function
  | Elt x -> [%synt base_case] x
  | Cons (hd, tl) ->
    if hd mod 2 = 0 && hd > 0 then [%synt f0] hd else [%synt f1] (lpen tl)
  [@@requires is_sorted]
;;

assert (amax = lpen)
