
let f0 a = a

let f1 b c = b + c

let rec target =
  function NInt(i) -> f0 i | NPlus(a, b) -> f1 (target a) (target b)

