(** @synduce --no-lifting *)
type clist = CNil | Single of int | Concat of int * clist * int * int * clist

type list = Nil | Cons of int * list

let rec spec = function
  | Nil -> (0, 0)
  | Cons (hd, tl) ->
      let mps, mss = spec tl in
      (max (mps + hd) 0, max mss (max (mps + hd) 0))
  [@@ensures fun (mps, mss) -> mps >= 0 && mss >= 0 && mss >= mps]

(** Memoize sum *)
let rec memo = 
  function
  | CNil -> true
  | Single x -> true
  | Concat(sx, x, my, sy, y) -> sx = sum x && sy = sum y && memo x && memo y && my = mps y
and sum = 
function 
| CNil -> 0 
| Single x -> x 
| Concat (sx, x, my, sy, y) -> sum x + sum y
and mps = function
  | CNil -> 0 
  | Single x -> x
  | Concat(sx,x,my,sy,y) -> max (mps x) (sx + (mps y))

let rec target t = h t [@@requires memo]

and h = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (sy, y, my, sz, z) -> [%synt odot] sy sz my (h y) (h z)

let rec repr t = c t

and c = function CNil -> Nil | Single a -> Cons (a, Nil) | Concat (sx, x, my, sy, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (sx, x, my, sy, y) -> dec (Concat (sx, y, my, sy, l)) x

