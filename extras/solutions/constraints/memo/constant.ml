
let xi_1 x0 = 1

let rec target =
  function MLeaf(a) -> 1 | MNode(n, a, l, r) -> n < 0 ? 2 : xi_1 n

