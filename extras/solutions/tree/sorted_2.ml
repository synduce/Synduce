
let f0 a b = b > a

let join c x y z = (y && z) && (x > c)

let rec h0 t = h x t
and h y=
  function Leaf(a) -> f0 a y | Node(a, l, r) -> join a y (h a l) (h a r)

