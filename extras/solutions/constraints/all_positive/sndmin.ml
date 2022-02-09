
let base_case a = (min a 0, min (max a 0) 0)

let init  = (0, 0)

let odot (b0, b1) (c0, c1) = (min b0 c0, min b1 (max b0 c0))

let rec target =
  function Empty -> init | Elt(a) -> base_case a
  | Concat(x, y) -> odot (target x) (target y)

