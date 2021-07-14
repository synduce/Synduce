
let s0  = (0, 0)

let join (j, j0) (j1, j2) = (max j (j0 + j1), j0 + j2)

let f0 x140 = (max x140 0, x140)

let rec hom  =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> join (hom x) (hom y)

