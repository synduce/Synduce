
let j0 a (b0, b1) (c0, c1) = (((b0 && c0) && (a > b1)) && (a > c1), a)

let s0 x = (x > 0, x)

let rec target =
  function Leaf(a) -> s0 a | Node(a, l, r) -> j0 a (target l) (target r)

