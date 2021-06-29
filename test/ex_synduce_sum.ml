type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec repr x = x

let rec spec t = f 0 t

and f s = function
  | Nil -> s
  | Node (a, l, r) ->
      let sum = f s l in
      f (sum + a) r

let s0 = 0

let join x2 x3 x4 = x3 + x2 + x4

let rec target = function Nil -> s0 | Node (a, l, r) -> join a (target l) (target r)
