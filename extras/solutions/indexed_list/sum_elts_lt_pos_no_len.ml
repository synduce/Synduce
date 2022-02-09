
let f a (b0, b1) = (a > b1 ? a + b0 : b0, b1 + 1)

let s0  = (0, 0)

let rec target = function Nil -> s0 | Cons(hdv, tl) -> f hdv (target tl)

