
let base_case x0 = x0 ≥ 0 ? x0 : 0

let oplus x1 x2 = x1 > (- x1) ? x2 + x1 : x2

let rec target =
  function Elt(x) -> base_case x | Cons(hd, tl) -> oplus hd (target tl)

