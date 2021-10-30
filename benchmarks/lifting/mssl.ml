(** @synduce *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec f = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let mps, mss = f tl in
    max (mps + hd) 0, max mss (max (mps + hd) 0)
  [@@ensures fun (mps, mss) -> mps >= 0 && mss >= 0 && mss >= mps]
;;

let rec target t = h t

and h = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (y, z) -> [%synt odot] (h y) (h z)
;;

let rec repr t = c t

and c = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;

assert (target = repr @@ f)
