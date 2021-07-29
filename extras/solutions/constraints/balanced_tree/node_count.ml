
let s0  = 0

let f0 x2 x3 = x3 + (x3 + 1)

let rec target  = function Nil -> s0 | Node(a, l, r) -> f0 a (target l)

