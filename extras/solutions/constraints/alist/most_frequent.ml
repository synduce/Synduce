
let s0 x19 x20 = (x20, x19)

let join x23 x24 (x25, x250) =
  (x24 > x25 ? x24 : x25, x24 > x25 ? x23 : x250)

let rec target  =
  function AElt(k, v) -> s0 k (int_of v)
  | ACons(hd_key, hdv, tl) -> join hd_key (int_of hdv) (target tl)
and int_of  = function S(n) -> 1 + (int_of n) | One -> 1

