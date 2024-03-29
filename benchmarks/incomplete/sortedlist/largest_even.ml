(** @synduce -s 2 -NB --no-lifting *)

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
  | Elt x -> x
  | Cons (hd, tl) ->
    let r = lpen tl in
    if hd mod 2 = 0 then max hd r else r
;;

let rec amax = function
  | Elt x -> x
  | Cons (hd, tl) -> if hd mod 2 = 0 then [%synt f0] else [%synt f1]
[@@requires is_sorted]
;;

assert (amax = lpen)
