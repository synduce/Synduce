type 'a clist =
  | CTwo of 'a * 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Two of 'a * 'a
  | Cons of 'a * 'a list

let rec spec l = f l

and f = function
  | Two (a, b) -> max a b, min a b
  | Cons (hd, tl) ->
    (fun (amax, sec_max) -> max hd amax, max sec_max (min hd amax)) (f tl)
  [@@ensures fun (x, y) -> x <= y]
;;

let rec target t = h t

and h = function
  | CTwo (a, b) -> [%synt f0] a b
  | Concat (x, y) -> [%synt odot] (h x) (h y)
;;

let rec repr l = c l

and c = function
  | CTwo (a, b) -> Two (a, b)
  | Concat (x, y) -> dec y x

and dec l = function
  | CTwo (a, b) -> Cons (a, Cons (b, c l))
  | Concat (x, y) -> dec (Concat (y, l)) x
;;
