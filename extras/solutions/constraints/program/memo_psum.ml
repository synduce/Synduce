
let f0 a = a

let f1 b = b

let f2 c = c

let rec target =
  function NInt(i) -> f0 i | NPlus(s, a, b) -> f1 s | NMinus(s, a, b) -> f2 s

