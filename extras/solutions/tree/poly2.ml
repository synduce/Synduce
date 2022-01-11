
let join a b (c0, c1) (x0, x1) =
  (x0 + (x1 × (b + (a × c0))), (a × c1) × x1)

let s0  = (0, 1)

let rec h = function Nil -> s0 | Node(a, l, r) -> join x a (h l) (h r)

