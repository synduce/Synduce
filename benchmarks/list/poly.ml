type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let spec x l =
  let rec f = function
    | Nil -> 0, 1
    | Cons (hd, tl) -> (fun (s, m) -> s + (hd * m), x * m) (f tl)
  in
  f l
;;

let target x t =
  let rec h = function
    | CNil -> [%synt s0]
    | Single a -> [%synt f0] x a
    | Concat (y, z) -> [%synt odot] (h y) (h z)
  in
  h t
;;

let rec repr = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;
