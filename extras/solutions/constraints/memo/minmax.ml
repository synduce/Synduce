
let f0 a = (a, a)

let join b c x y z = (min (min b c) y, max (max b x) z)

let rec target =
  function Leaf(x) -> f0 x
  | Node(lmin, lmax, rmin, rmax, val_, l, r) -> join val_ lmin lmax rmin rmax

