
let f0 x44 x45 = x45 + (- (x44 + (- 1)))

let s0 x46 = 0

let rec target  =
  function Nil -> s0 1 | Node(a, l, r) -> a + (f0 a (target l))

