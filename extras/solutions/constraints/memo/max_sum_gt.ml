
let s0 x117 x118 = x118 > x117 ? x118 : 0

let f1 x119 x120 x121 x122 = (x121 + x122) + (x120 > x119 ? x120 : 0)

let c0  = (0 + 0) + 0

let rec g  =
  function MLeaf(a) -> s0 input a
  | MNode(n, a, l, r) -> input > n ? c0 : f1 input a (g l) (g r)

