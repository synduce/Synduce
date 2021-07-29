
let s0 x6 = 0

let f1 x7 x8 = x8

let f0 x9 = x9

let rec g  =
  function ANil -> s0 a
  | ACons(hd_key, tl) -> hd_key = a ? f0 hd_key : f1 hd_key (g tl)

