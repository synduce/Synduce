
let s0 x30 = 0

let f1 x31 x32 = x32

let f0 x33 = x33

let rec g =
  function ANil -> s0 a
  | ACons(hd_key, tl) -> hd_key = a ? f0 hd_key : f1 hd_key (g tl)

