
let c0  = 1

let c1  = 0

let f1 x130 = x130

let f0 x131 = x131

let rec target =
  function MLeaf(n, a) -> a < 2 ? c0 : c1
  | MNode(n, a, l, r) -> a < 2 ? f0 n : f1 n

