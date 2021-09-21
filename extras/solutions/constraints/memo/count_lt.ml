
let c0  = 1

let c1  = 0

let f1 x4 = x4

let f0 x5 = x5

let rec target =
  function MLeaf(n, a) -> a < 2 ? c0 : c1
  | MNode(n, a, l, r) -> a < 2 ? f0 n : f1 n

