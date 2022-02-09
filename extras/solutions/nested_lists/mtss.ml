
let s0 a = (max a 0, a)

let s1 (b0, b1) (c0, c1) = (max c0 (b0 + c1), b1 + c1)

let rec target =
  function Sglt(x) -> s0 (tsum x) | Cat(l, r) -> s1 (target r) (target l)
and tsum = function Elt(x) -> x | Cons(hd, tl) -> hd + (tsum tl)

