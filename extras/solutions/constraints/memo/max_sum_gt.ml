
let s0 x32 x33 = x33 > x32 ? x33 : 0

let f1 x34 x35 x36 x37 = (x36 + x37) + (x35 > x34 ? x35 : 0)

let c0  = (0 + 0) + 0

let rec g =
  function MLeaf(a) -> s0 input a
  | MNode(n, a, l, r) -> input > n ? c0 : f1 input a (g l) (g r)

