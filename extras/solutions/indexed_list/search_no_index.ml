
let f a b (c0, c1) = (a = b ? c1 : c0, c1 + 1)

let s0  = (0, 0)

let rec bux = function Nil -> s0 | Cons(hdv, tl) -> f x hdv (bux tl)

