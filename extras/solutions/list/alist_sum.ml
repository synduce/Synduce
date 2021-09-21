
let s0  = 0

let f0 x0 (x10, x11) = let (key, value) = (x10, x11) in
                       x0 = key ? value : 0

let join x2 x3 = x2 + x3

let rec h =
  function Empty -> s0 | Single(a) -> f0 x a
  | Concat(l, r) -> join (h r) (h l)

