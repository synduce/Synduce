(** @synduce --no-lifting -n 10 -NB *)

(* When using args in comments, they must be separated by at least one line from the declarations. *)

type arithop = APlus | AMinus | AGt

type boolop = BNot | BAnd | BOr | BEq

type term =
  | TArithBin of arithop * term * term
  | TBoolBin of boolop * term * term
  | TArithUn of arithop * term
  | TBoolUn of boolop * term
  | TVar of int
  | TCInt of int
  | TCBool of bool

type op = Plus | Minus | Not | And | Or | Gt | Eq

type term2 =
  | Bin of op * term2 * term2
  | Un of op * term2
  | Var of int
  | CInt of int
  | CBool of bool

let rec repr = function
  | Bin (o, a, b) -> mk_bin a b o
  | Un (o, x) -> mk_un x o
  | Var i -> TVar i
  | CInt i -> TCInt i
  | CBool b -> TCBool b

and mk_bin a b = function
  | Plus -> TArithBin (APlus, repr a, repr b)
  | Minus -> TArithBin (AMinus, repr a, repr b)
  | Not -> TBoolBin (BNot, repr a, repr b)
  | And -> TBoolBin (BAnd, repr a, repr b)
  | Or -> TBoolBin (BOr, repr a, repr b)
  | Gt -> TArithBin (AGt, repr a, repr b)
  | Eq -> TBoolBin (BEq, repr a, repr b)

and mk_un a = function
  | Plus -> TArithUn (APlus, repr a)
  | Minus -> TArithUn (AMinus, repr a)
  | Not -> TBoolUn (BNot, repr a)
  | And -> TBoolUn (BAnd, repr a)
  | Or -> TBoolUn (BOr, repr a)
  | Gt -> TArithUn (AGt, repr a)
  | Eq -> TBoolUn (BEq, repr a)

let rec well_formed_term = function
  | Bin (op, a, b) -> well_formed_term a && well_formed_term b && is_binary op
  | Un (op, a) -> well_formed_term a && is_unary op
  | Var i -> true
  | CInt i -> true
  | CBool b -> true

and is_binary = function
  | Plus -> true
  | Minus -> true
  | And -> true
  | Or -> true
  | Gt -> true
  | Eq -> true
  | Not -> false

and is_unary = function
  | Plus -> false
  | Minus -> false
  | And -> false
  | Or -> false
  | Gt -> false
  | Eq -> false
  | Not -> true

let rec spec = function
  | TArithBin (a, b, c) -> spec c + spec b
  | TBoolBin (a, b, c) -> count a + spec c + spec b
  | TArithUn (o, a) -> spec a
  | TBoolUn (o, b) -> spec b + count o
  | TVar i -> 0
  | TCInt i -> 0
  | TCBool b -> 0

and count = function BNot -> 1 | BAnd -> 0 | BOr -> 0 | BEq -> 0

(* The constraint on well formedness does nothing in this example. *)
let rec target = function
  | Bin (op, a, b) -> [%synt hbin] (target a) (target b)
  | Un (op, a) -> [%synt hun] (target a) (bcount op)
  | Var i -> [%synt var]
  | CInt i -> [%synt const]
  | CBool b -> [%synt boolconst]
  [@@requires well_formed_term]

and bcount = function
  | Plus -> [%synt f_op_plus]
  | Minus -> [%synt f_op_minus]
  | And -> [%synt f_op_and]
  | Or -> [%synt f_op_or]
  | Gt -> [%synt f_op_gt]
  | Eq -> [%synt f_op_eq]
  | Not -> [%synt f_op_not]
