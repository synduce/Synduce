
let xi_0 x6 = x6

let xi_1 x7 x8 = x8

let rec target =
  function Leaf(a) -> xi_0 a | Node(a, l, r) -> xi_1 a (target r)

