
let f0 x7 x8 = x8 + (- (x7 + (- 1)))

let s0 x9 = 0

let rec target =
  function Nil -> s0 1 | Node(a, l, r) -> a + (f0 a (target l))

