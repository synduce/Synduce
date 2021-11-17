(** @synduce --no-lifting *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec fbal = function
  | Nil -> 0, 0, true
  | Cons (hd, tl) ->
    let cnt, min_cnt, bal = fbal tl in
    let cnt = if hd then cnt + 1 else cnt - 1 in
    cnt, min min_cnt cnt, bal && cnt >= 0
  [@@ensures
    fun (cnt, min_cnt, bal) -> min_cnt <= cnt && min_cnt <= 0 && bal = (min_cnt >= 0)]
;;

let rec target = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)
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

assert (target = repr @@ fbal)
