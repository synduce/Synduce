open Base
open Term

(**
  Left distributive operators: for each pair op1, op2 it means that:
  a op1 (b op2 c) = (a op1 b) op2 (a op1 c)
*)
let left_distrib = Binop.[ (Plus, Max); (Binop.Times, Plus); (Plus, Min) ]

(**
  Right distributive operators: for each pair op1, op2 it means that:
  (b op2 c) op1 a = (b op1 a) op2 (c op1 a)
*)
let right_distrib = Binop.[ (Plus, Max); (Times, Plus); (Div, Plus); (Plus, Min) ]
