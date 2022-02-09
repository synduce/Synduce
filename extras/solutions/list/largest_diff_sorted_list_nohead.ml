
let g0 a = (0, - a)

let join b (c0, c1) = (max (max c0 (abs (b + c1))) 0, - b)

let rec amax = function Elt(x) -> g0 x | Cons(hd, tl) -> join hd (amax tl)

