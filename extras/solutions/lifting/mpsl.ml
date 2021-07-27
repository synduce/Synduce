
let s0  = (0, 0)

let join (x136, x1360) (x137, x1370) =
  (max x136 (x1360 + x137), x1360 + x1370)

let f0 x140 = (max x140 0, x140)

let rec hom  =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> join (hom x) (hom y)

