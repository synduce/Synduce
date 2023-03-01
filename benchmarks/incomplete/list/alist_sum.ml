(** @synduce -s 2 -NB *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a clist =
  | Empty
  | Single of 'a
  | Concat of 'a clist * 'a clist

let rec repr = function
  | Empty -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Empty -> repr l1
  | Single a -> Cons (a, repr l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let spec x l =
  let rec f = function
    | Nil -> 0
    | Cons (hd, tl) ->
      let key, value = hd in
      if x = key then value + f tl else f tl
  in
  f l
;;

let target x l =
  let rec h = function
    | Empty -> [%synt s0]
    | Single a -> [%synt f0]
    | Concat (l, r) -> [%synt join]
  in
  h l
;;
