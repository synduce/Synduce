
let s0  = 0

let f0 x5 x6 = x5 + (x6 + x6)

let rec sum2 = function Null -> s0 | Node(a, l, r) -> f0 a (sum2 l)

