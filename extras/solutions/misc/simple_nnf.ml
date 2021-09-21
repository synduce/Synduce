
let eor x4 x5 = x4 || x5

let eand x2 x3 = x2 && x3

let eneglit x1 = ¬ x1

let elit x0 = x0

let rec eval2 =
  function NFLit(b) -> elit b | NFNegLit(b) -> eneglit b
  | NFAnd(x, y) -> eand (eval2 x) (eval2 y)
  | NFOr(x, y) -> eor (eval2 x) (eval2 y)

