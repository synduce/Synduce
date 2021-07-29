
let s0 x6 = 0

let f1 x7 x8 = x8

let f0 x9 = x9

let rec g  =
  function ANil -> s0 a
  | ACons(hd_key, hdv, tl) -> hd_key = a ? f0 (int_of hdv) : f1 hd_key (g tl)
and int_of  = function S(n) -> 1 + (int_of n) | Z -> 0

