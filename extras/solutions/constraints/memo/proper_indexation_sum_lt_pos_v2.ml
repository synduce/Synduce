
let f a b (c0, c1) = (a > c1 ? max (a + c0) 0 : c0, c1 + 1)

let s0  = (0, 0)

let rec target =
  function CNil -> s0 | CCons(hdv, hdi, tl) -> f hdv hdi (target tl)

