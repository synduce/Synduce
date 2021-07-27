
let s0  = (0, 0)

let join1 x156 (x157, x1570) (x158, x1580) =
  (max x158 (x156 + (x157 + x1580)), x156 + (x1570 + x1580))

let rec mits  =
  function Nil -> s0 | Node(a, l, r) -> join1 a (mits l) (mits r)

