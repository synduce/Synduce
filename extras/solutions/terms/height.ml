
let boolconst  = 1

let const  = 1

let var  = 1

let hun x6 = x6

let hbin x19 x20 = max x20 x19

let rec target =
  function Bin(op, a, b) -> hbin (target a) (target b)
  | Un(op, a) -> hun (target a) | Var(i) -> var | CInt(i) -> const
  | CBool(b) -> boolconst

