type 'a list = Nil | Cons of 'a * 'a list

let rec spec = function Nil -> 0 | Cons (hd, tl) -> hd + spec tl

let rec target l = f [%synt s0] l

and f s = function Nil -> s | Cons (hd, tl) -> f ([%synt oplus] s hd) tl
