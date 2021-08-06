
let s0 x29 x30 = (x30, x29)

let join x33 (x34, x340) (x35, x350) =
  (x34 > x35 ? x34 : x35, x35 > x34 ? x350 : x340)

let rec target =
  function KeyValue(k, v) -> s0 k (int_of v)
  | Node(hd_key, l, r) -> join hd_key (target l) (target r)
and int_of = function S(n) -> 1 + (int_of n) | One -> 1

