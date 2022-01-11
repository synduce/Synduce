
let s0 a b = a + b

let s1 c = c

let rec target =
  function Single(a, b) -> s0 a b | Concat(l, r) -> s1 (target r)

