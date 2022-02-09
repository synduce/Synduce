
let f a b c = a > b ? a + c : c

let s0  = 0

let rec target =
  function CNil -> s0 | CCons(hdv, hdi, tl) -> f hdv hdi (target tl)

