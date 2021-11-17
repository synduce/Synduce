type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

type sel =
  | Left
  | Right

type 'c zipper =
  | Top
  | Zip of sel * 'c * 'c btree * 'c zipper

let rec spec t = mips (0, 0) t

and mips s = function
  | Empty -> s
  | Node (a, l, r) ->
    let sum1, m1 = mips s l in
    mips (sum1 + a, max (sum1 + a) m1) r
  [@@ensures fun (x, y) -> y >= 0 && y >= x]
;;

let rec target = function
  | Top -> [%synt s0]
  | Zip (c, a, child, z) -> aux a child z c

and aux a child z = function
  | Left -> [%synt joinl] a (spec child) (target z)
  | Right -> [%synt joinr] a (spec child) (target z)
;;

let rec repr = function
  | Top -> Empty
  | Zip (w, lbl, child, z) -> h lbl child z w

and h lbl child z = function
  | Left -> Node (lbl, child, repr z)
  | Right -> Node (lbl, repr z, child)
;;
