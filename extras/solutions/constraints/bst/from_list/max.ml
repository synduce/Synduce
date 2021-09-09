
let xi_0 x1 = x1

let xi_1 x2 x3 = x3

let rec target =
  function Leaf(a) -> xi_0 a | Node(a, l, r) -> xi_1 a (target r)

