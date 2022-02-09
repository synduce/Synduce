
let xi_0 a = (a, a)

let xi_2 b c x y z = (max (max b x) y, min (min b c) z)

let rec target =
  function Leaf(a) -> xi_0 a
  | Node(a, l, r) -> xi_2 a (amin l) (amax r) (amax l) (amin r)
and amin =
  function Leaf(a) -> a | Node(a, l, r) -> min a (min (amin l) (amin r))
and amax =
  function Leaf(a) -> a | Node(a, l, r) -> max a (max (amax l) (amax r))

