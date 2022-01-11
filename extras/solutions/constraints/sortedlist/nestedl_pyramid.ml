
let s0 (a0, a1) b = (min a0 a0, a1, true)

let s1 c (x0, x1, x2) =
  (min c x0, max c x1, (x2 && (c ≥ x0)) && (c ≥ x1))

let rec target =
  function Line(x) -> s0 (inter x) true
  | NCons(hd, tl) -> s1 (plmin hd) (target tl)
and inter =
  function Elt(x) -> (x, x)
  | Cons(hd, tl) -> let (lo, hi) = inter tl in
                    (min hd lo, max hd hi)
and plmin =
  function Elt(x) -> x | Cons(hd, tl) -> let lo = plmin tl in
                                         min hd lo

