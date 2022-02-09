
let f a (b0, b1) = (b0 + (a × b1), b1 + 1)

let s0  = (0, 0)

let rec target = function Nil -> s0 | Cons(hdv, tl) -> f hdv (target tl)

