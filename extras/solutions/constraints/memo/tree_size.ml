
let xi_0  = 1

let xi_1 x = x

let rec target  = function MLeaf(a) -> xi_0 | MNode(n, a, l, r) -> xi_1 n
