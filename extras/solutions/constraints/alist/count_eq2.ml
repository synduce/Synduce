
let s0 x34 = 0

let f1 x35 x36 = x36

let f0 x37 = x37

let rec g  =
  function ANil -> s0 a
  | ACons(hd_key, hdv, tl) -> hd_key = a ? f0 (int_of hdv) : f1 hd_key (g tl)
and int_of  = function S(n) -> 1 + (int_of n) | Z -> 0

