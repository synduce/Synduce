
let s0  = (0, 0)

let join1 x156 (j, j0) (j1, j2) =
  (max j1 (x156 + (j + j2)), x156 + (j0 + j2))

let rec mits  =
  function Nil -> s0 | Node(a, l, r) -> join1 a (mits l) (mits r)

