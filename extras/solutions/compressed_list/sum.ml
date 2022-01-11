
let f a b c = c + (a × b)

let s0  = 0

let rec target =
  function CNil -> s0 | CCons(hdc, hdv, tl) -> f (value hdc) hdv (target tl)
and value = function Z -> 0 | S(n) -> 1 + (value n)

