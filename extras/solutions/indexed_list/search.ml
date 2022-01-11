
let f a b c x = a = b ? c : x

let s0  = 0

let rec bux =
  function Nil -> s0 | Cons(hdv, tl) -> f x hdv (length tl) (bux tl)
and length = function Nil -> 0 | Cons(_, tl) -> 1 + (length tl)

