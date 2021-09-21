
let sub1 x20 = (- 1) + x20

let add1 x21 = 1 + x21

let add x22 x23 = (10 + x22) + x23

let c0  = - 5

let rec two_esum _x =
          match _x with TwoInts(i1, i2) -> add (to_int i1) (to_int i2)
and to_int =
  function Zero -> c0 | Sub1(n) -> sub1 (to_int n)
  | Add1(n) -> add1 (to_int n)

