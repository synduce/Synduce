
let s0 x37 x38 = x37 = x38

let f1 x39 x40 x41 x42 = (x39 = x40) || (x41 || x42)

let c0 x43 x44 = x44 > x44

let rec g  =
  function MLeaf(a) -> s0 x a
  | MNode(n, a, l, r) -> x > n ? c0 x a : f1 x a (g l) (g r)

