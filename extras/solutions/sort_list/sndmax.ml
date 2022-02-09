
let base_case  = (0, 0)

let oplus a (b0, b1) = (min b1 (max a b0), max a b1)

let rec target =
  function Nil -> base_case | Cons(hd, tl) -> oplus hd (target tl)

