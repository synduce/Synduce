(** @synduce --max-lifting=1 *)

type blist = Nil | Cons of bool * blist

type clist = Emp | Single of bool | Concat of clist * clist

(* Checks some substring matches 10* *)
let rec spec = function
  | Nil -> (false, false)
  | Cons (hd, tl) ->
      let seen1, res = spec tl in
      (seen1 || hd, (seen1 && not hd) || res)

let rec repr = function Emp -> Nil | Single a -> Cons (a, Nil) | Concat (x, y) -> dec y x

and dec l = function
  | Emp -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x

let rec target = function
  | Emp -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)
