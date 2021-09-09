
let xi_0 x2 = x2

let xi_1 x3 x4 = x4

let rec target =
  function Leaf(a) -> xi_0 a | Node(a, l, r) -> xi_1 a (target r)

