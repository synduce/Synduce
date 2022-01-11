
let base_case a = (max a 0, max (min a 0) 0)

let init  = (0, 0)

let odot (b0, b1) (c0, c1) = (max b0 c0, max b1 (min c0 (max b0 c1)))

let rec target =
  function Empty -> init | Elt(a) -> base_case a
  | Concat(x, y) -> odot (target x) (target y)

