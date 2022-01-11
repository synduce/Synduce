
let xi_0  = 0

let xi_1 a = a

let xi_2 b c = b + c

let rec target =
  function Nil -> xi_0 | Leaf(a) -> xi_1 a
  | Node(a, l, r) -> xi_2 a (target l)

