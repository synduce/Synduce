(** @synduce -s 2 -NB -n 20 --no-lifting *)

type arith_expr =
  | Int of int
  | Plus of arith_expr * arith_expr
  | Minus of arith_expr * arith_expr

type norm_expr =
  | NInt of int
  | NPlus of norm_expr * norm_expr

let rec repr = function
  | NInt i -> Int i
  | NPlus (a, b) -> Plus (repr a, repr b)
;;

let rec spec = function
  | Int i -> i
  | Plus (a, b) -> spec a + spec b
  | Minus (a, b) -> spec a - spec b
;;

let rec target = function
  | NInt i -> [%synt f0] i
  | NPlus (a, b) -> [%synt f1] (target a) (target b)
;;
