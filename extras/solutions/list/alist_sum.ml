
let s0  = 0

let f0 x (x0, x00) = let (key, value) = (x0, x00) in
                     x = key ? value : 0

let join x1 x2 = x1 + x2

let rec h =
  function Empty -> s0 | Single(a) -> f0 x a
  | Concat(l, r) -> join (h r) (h l)

