
let s0 x44 x45 = x44 = x45

let f1 x46 x47 x48 x49 = (x46 = x47) || (x48 || x49)

let c0 x50 x51 = x51 > x50

let rec g =
  function MLeaf(a) -> s0 x a
  | MNode(n, a, l, r) -> x > n ? c0 x a : f1 x a (g l) (g r)

