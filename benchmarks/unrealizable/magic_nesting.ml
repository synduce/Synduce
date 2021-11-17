(** @synduce -NBc --no-lifting *)

type list =
  | Elt of int * int
  | Cons of int * list

let rec spec = function
  | Elt (a, b) -> b
  | Cons (hd, tl) ->
    let a = spec tl in
    hd
;;

let rec target = function
  | Elt (a, b) -> [%synt f0] (a, b)
  | Cons (hd, tl) -> [%synt f0] (hd, [%synt f0] (hd, target tl))
;;
