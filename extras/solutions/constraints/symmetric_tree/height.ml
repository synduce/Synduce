
let s0  = 0

let f0 x5 x6 = x6 + (x6 + 1)

let rec height2 = function Null -> s0 | Node(a, l, r) -> f0 a (height2 l)

