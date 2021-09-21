
let s0  = (0, 0)

let join (x1150, x1151) (x1160, x1161) =
  (max (x1160 + x1151) x1150, x1161 + x1151)

let f0 x113 = (max x113 0, x113)

let rec hom =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> join (hom x) (hom y)

