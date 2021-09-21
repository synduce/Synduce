
let int_succ x61 = 1 + x61

let int_base  = - (- 1)

let s0 x62 x63 = (x63, x62)

let join x55 (x560, x561) (x570, x571) =
  (x560 > x570 ? x560 : x570, x570 > x560 ? x571 : x561)

let rec target =
  function KeyValue(k, v) -> s0 k (int_of v)
  | Node(hd_key, l, r) -> join hd_key (target l) (target r)
and int_of = function S(n) -> int_succ (int_of n) | One -> int_base

