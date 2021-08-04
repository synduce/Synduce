
let base_case x = x ≥ 0 ? x : 0

let oplus x0 x1 = x0 > (- x0) ? x1 + x0 : x1

let rec target  =
  function Elt(x) -> base_case x | Cons(hd, tl) -> oplus hd (target tl)

