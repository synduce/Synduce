
let s0 x8 = 0

let f1 x9 x10 = x10

let f0 x11 = 1

let rec g =
  function ANil -> s0 a
  | ACons(hd_key, tl) -> hd_key = a ? f0 hd_key : f1 hd_key (g tl)

