
let s0 x8 = 0

let f1 x9 x10 = x10

let f0 x11 = x11

let rec g =
  function ANil -> s0 a
  | ACons(hd_key, hdv, tl) -> hd_key = a ? f0 (int_of hdv) : f1 hd_key (g tl)
and int_of = function S(n) -> 1 + (int_of n) | Z -> 0

