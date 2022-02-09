
let join a b (c0, c1) = (max b c0, b > c0 ? a : c1)

let s0 x y = (y, x)

let rec target =
  function AElt(k, v) -> s0 k (int_of v)
  | ACons(hd_key, hdv, tl) -> join hd_key (int_of hdv) (target tl)
and int_of = function S(n) -> 1 + (int_of n) | One -> 1

