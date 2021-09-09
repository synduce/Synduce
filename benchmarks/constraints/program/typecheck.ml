type 'a list = Elt of 'a | Cons of 'a * 'a list

type constant = ConstantInt of int | ConstantBool of bool

type binary_op = Plus | Times | And | Or | GreaterThan | LessThan | Equal

type unary_op = Minus | Not

type expression =
  | Constant of constant
  | Identifier of string
  | BinaryExpression of binary_op * expression * expression
  | UnaryExpression of unary_op * expression
  | If of expression * expression * expression

type statement =
  | While of expression * statement
  | Declaration of string * expression
  | Return of expression
  | Throw

type program = Program of statement list | EmptyProgram

(* Type invariant says there are no boolean constants or operators in this program *)
let rec no_bool = function Program stmts -> no_bool_stmts stmts | EmptyProgram -> true

and no_bool_stmts = function
  | Elt stmt -> no_bool_stmt stmt
  | Cons (a, l) -> no_bool_stmt a && no_bool_stmts l

and no_bool_stmt = function
  | While (a, s) -> false
  | Declaration (s, e) -> no_bool_expr e
  | Return e -> no_bool_expr e
  | Throw -> true

and no_bool_expr = function
  | Constant c -> no_bool_const c
  | Identifier s -> true
  | BinaryExpression (o, l, r) -> no_bool_expr l && no_bool_expr r && no_bool_binop o
  | UnaryExpression (o, e) -> no_bool_expr e && no_bool_unop o
  | If (c, l, r) -> false

and no_bool_const = function ConstantBool b -> false | ConstantInt i -> true

and no_bool_binop = function
  | Plus -> true
  | Times -> true
  | And -> false
  | Or -> false
  | GreaterThan -> false
  | LessThan -> false
  | Equal -> false

and no_bool_unop = function Minus -> true | Not -> false

(* Reference function does simplistic type checking *)
let rec spec = function Program a -> aux_list a | EmptyProgram -> 1
  [@@ensures fun x -> x = 0 || x = 1]

and aux_list = function
  | Elt a -> aux_stmt a
  | Cons (a, b) -> if aux_stmt a = 1 && aux_list b = 1 then 1 else 0

and aux_stmt = function
  | While (a, b) -> if aux_expr a = 1 && aux_type_expr a = 1 && aux_stmt b = 1 then 1 else 0
  | Declaration (v, e) -> aux_expr e
  | Return e -> aux_expr e
  | Throw -> 1

and aux_expr = function
  | Constant c -> 1
  | Identifier s -> 1
  | BinaryExpression (o, l, r) ->
      if
        (aux_type_binop_operand o = 3 && aux_type_expr l = aux_type_expr r)
        || aux_type_expr l > 0
           && aux_type_expr r > 0
           && aux_type_expr l = aux_type_expr r
           && aux_type_expr l = aux_type_binop_operand o
      then 1
      else 0
  | UnaryExpression (o, e) ->
      if aux_type_expr e = 3 || (aux_type_expr e > 0 && aux_type_unop o = aux_type_expr e) then 1
      else 0
  | If (c, l, r) -> if aux_type_expr c = 1 && aux_type_expr l = aux_type_expr r then 1 else 0

(* Types: error -> 0 , bool -> 1, int -> 2, any/unknown -> 3 *)
and aux_type_expr = function
  | Constant c -> aux_type_const c
  | Identifier s -> 3
  | BinaryExpression (o, l, r) ->
      if aux_expr (BinaryExpression (o, l, r)) > 0 then aux_type_binop_ret o else 0
  | UnaryExpression (o, e) -> if aux_expr (UnaryExpression (o, e)) > 0 then aux_type_unop o else 0
  | If (c, l, r) ->
      if aux_expr (If (c, l, r)) > 0 then if aux_type_expr c = 1 then aux_type_expr l else 0 else 0

and aux_type_binop_operand = function
  | Plus -> 2
  | Times -> 2
  | And -> 1
  | Or -> 1
  | GreaterThan -> 2
  | LessThan -> 2
  | Equal -> 3

and aux_type_binop_ret = function
  | Plus -> 2
  | Times -> 2
  | And -> 1
  | Or -> 1
  | GreaterThan -> 1
  | LessThan -> 1
  | Equal -> 1

and aux_type_unop = function Minus -> 2 | Not -> 1

and aux_type_const = function ConstantBool b -> 1 | ConstantInt i -> 2

(* Target function should synthesize constant `1` because all expressions are either int or bool and programs without bool constants or operators thus may not have type errors *)
let rec target = function Program a -> [%synt s0] | EmptyProgram -> [%synt s1]
  [@@requires no_bool]
