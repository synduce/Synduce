
let odot (x120, x121, x122) (x130, x131, x132) =
  (x120 && x130, (((¬ x122) || x130) && x121) && x131, x132)

let f0 x9 = (x9, true, x9)

let rec target =
  function Single(a) -> f0 a | Concat(y, z) -> odot (target y) (target z)

