type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec lpeak = function
  | Nil -> (0, true, 0, 0)
  | Cons (hd, tl) ->
      let cnt, aux1, aux2, lp = lpeak tl in
      let new_cnt = if hd >= 0 then cnt + hd else 0 in
      let new_aux = aux1 && hd >= 0 in
      (new_cnt, new_aux, (if new_aux then aux2 + hd else aux2), max lp new_cnt)
  [@@ensures fun (cnt, aux1, aux2, lp) -> cnt >= 0 && lp >= cnt && aux2 >= 0]

let rec target = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)

let rec repr = function CNil -> Nil | Single a -> Cons (a, Nil) | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;

assert (target = repr @@ lpeak)
