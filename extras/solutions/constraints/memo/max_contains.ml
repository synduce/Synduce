
let s0 x117 x118 = x117 = x118

let f1 x119 x120 x121 x122 = (x119 = x120) || (x121 || x122)

let c0 x123 x124 = x124 > x124

let rec g =
  function MLeaf(a) -> s0 x a
  | MNode(n, a, l, r) -> x > n ? c0 x a : f1 x a (g l) (g r)

