
let f0 a = (a, false, ¬ a)

let odot (b0, b1, b2) (c0, c1, c2) =
  (b0 || c0, (b1 || c1) || (b2 && c0), b2 || c2)

let s0  = (false, false, false)

let rec target =
  function Emp -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> odot (target x) (target y)

