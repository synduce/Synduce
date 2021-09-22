
let s0  = (0, 0)

let join (x1250, x1251) (x1260, x1261) =
  (max (x1260 + x1251) x1250, x1251 + x1261)

let f0 x119 = (max x119 0, x119)

let rec hom =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> join (hom x) (hom y)

