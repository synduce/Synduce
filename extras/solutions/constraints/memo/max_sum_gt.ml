
let s0 x127 x128 = x128 > x127 ? x128 : 0

let f1 x129 x130 x131 x132 = (x131 + x132) + (x130 > x129 ? x130 : 0)

let c0  = (0 + 0) + 0

let rec g =
  function MLeaf(a) -> s0 input a
  | MNode(n, a, l, r) -> input > n ? c0 : f1 input a (g l) (g r)

