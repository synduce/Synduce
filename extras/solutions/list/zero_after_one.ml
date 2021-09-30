
let odot (x120, x121, x122) (x130, x131, x132) =
  (x130 || x120, ((x121 || x130) && x122) || x131, x132 || x122)

let s0  = (false, false, false)

let f0 x9 = (x9, false, ¬ x9)

let rec target =
  function Emp -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> odot (target x) (target y)

