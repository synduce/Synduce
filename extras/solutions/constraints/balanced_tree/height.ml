
let s0  = 0

let f0 x1 = x1 + 1

let rec target  = function Nil -> s0 | Node(a, l, r) -> f0 (target l)

