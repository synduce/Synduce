
let f a b c = a > b ? a + c : c

let s0  = 0

let rec target =
  function Nil -> s0 | Cons(hdv, tl) -> f hdv (length tl) (target tl)
and length = function Nil -> 0 | Cons(_, tl) -> 1 + (length tl)

