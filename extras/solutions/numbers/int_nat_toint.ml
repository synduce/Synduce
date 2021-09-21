
let c0  = 0

let sub1 x4 = x4 + (- 1)

let add1 x5 = x5 + 1

let rec etoint =
  function Zero -> c0 | Sub1(n) -> sub1 (etoint n)
  | Add1(n) -> add1 (etoint n)

