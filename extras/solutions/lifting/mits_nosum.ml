
let s0  = (0, 0)

let join1 x184 (x1850, x1851) (x1860, x1861) =
  (max ((x1861 + x1850) + x184) x1860, (x1851 + x184) + x1861)

let rec mits =
  function Nil -> s0 | Node(a, l, r) -> join1 a (mits l) (mits r)

