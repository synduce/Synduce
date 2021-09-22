
let int_succ x81 = 1 + x81

let int_base  = - (- 1)

let s0 x82 x83 = (x83, x82)

let join x75 (x760, x761) (x770, x771) =
  (x760 > x770 ? x760 : x770, x770 > x760 ? x771 : x761)

let rec target =
  function KeyValue(k, v) -> s0 k (int_of v)
  | Node(hd_key, l, r) -> join hd_key (target l) (target r)
and int_of = function S(n) -> int_succ (int_of n) | One -> int_base

