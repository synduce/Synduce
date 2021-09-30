
let s0 x0 x1 = x1 > x0 ? x1 : 0

let f1 x2 x3 x4 x5 = (x4 + x5) + (x3 > x2 ? x3 : 0)

let c0  = 0

let rec g =
  function MLeaf(a) -> s0 input a
  | MNode(n, a, l, r) -> input > n ? c0 : f1 input a (g l) (g r)

