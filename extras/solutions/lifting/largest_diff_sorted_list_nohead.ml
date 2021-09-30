
let join x37 (x380, x381) = (max x380 (x37 + x381), - x37)

let g0 x35 = (0, - x35)

let rec amax = function Elt(x) -> g0 x | Cons(hd, tl) -> join hd (amax tl)

