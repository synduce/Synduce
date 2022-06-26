(** @synduce -s 2 --no-lifting -lNB -n 40 *)

type 'a clist =
  | CTwo of 'a * 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Two of 'a * 'a
  | Cons of 'a * 'a list

let rec repr l = c l

and c = function
  | CTwo (a, b) -> Two (a, b)
  | Concat (x, y) -> dec y x

and dec l = function
  | CTwo (a, b) -> Cons (a, Cons (b, c l))
  | Concat (x, y) -> dec (Concat (y, l)) x
;;

let rec spec l = f l

and f = function
  | Two (a, b) -> min a b, max a b
  | Cons (hd, tl) ->
    (fun (amin, sec_min) -> min hd amin, min sec_min (max hd amin)) (f tl)
  [@@ensures fun (x, y) -> x <= y]
;;

let rec is_sorted l = (fun (a, b) -> b) (sorted (repr l))

and sorted = function
  | Two (x, y) -> x, x > y
  | Cons (a, l) ->
    let hd, sorted = sorted l in
    a, a > hd && sorted
;;

let rec target = function
  | CTwo (a, b) -> [%synt f0] a b
  | Concat (x, y) -> [%synt odot] (target y)
  [@@requires is_sorted]
;;
