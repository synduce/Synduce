
let s0 x20 x21 = (x21, x20)

let join x24 x25 (x260, x261) =
  (x25 > x260 ? x25 : x260, x25 > x260 ? x24 : x261)

let rec target =
  function AElt(k, v) -> s0 k (int_of v)
  | ACons(hd_key, hdv, tl) -> join hd_key (int_of hdv) (target tl)
and int_of = function S(n) -> 1 + (int_of n) | One -> 1

