
let inner0 a = (a, a)

let inner1 b (c0, c1) = (min b c0, max b c1)

let s1 (x0, x1) (y0, y1) = (min x0 y0, max x1 y1)

let rec target =
  function Sglt(x) -> inner x | Cat(l, r) -> s1 (target r) (target l)
and inner =
  function Elt(x) -> inner0 x | Cons(hd, tl) -> inner1 hd (inner tl)

