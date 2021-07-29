
let s0 x37 x38 = x38 > x37 ? x38 : 0

let f1 x39 x40 x41 x42 = (x41 + x42) + (x40 > x39 ? x40 : 0)

let c0  = (0 + 0) + 0

let rec g  =
  function MLeaf(a) -> s0 input a
  | MNode(n, a, l, r) -> input > n ? c0 : f1 input a (g l) (g r)

