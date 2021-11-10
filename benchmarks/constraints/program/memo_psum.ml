(** @synduce -NB -n 20 --no-lifting *)

type arith_expr =
  | Int of int
  | Plus of arith_expr * arith_expr
  | Minus of arith_expr * arith_expr

type psum_expr =
  | NInt of int
  | NPlus of int * psum_expr * psum_expr
  | NMinus of int * psum_expr * psum_expr

let rec repr = function
  | NInt i -> Int i
  | NPlus (s, a, b) -> Plus (repr a, repr b)
  | NMinus (s, a, b) -> Minus (repr a, repr b)
;;

let rec is_memo_psum = function
  | NInt i -> true
  | NPlus (s, a, b) -> s = sum a + sum b
  | NMinus (s, a, b) -> s = sum a - sum b

and sum = function
  | NInt i -> i
  | NPlus (s, a, b) -> sum a + sum b
  | NMinus (s, a, b) -> sum a - sum b
;;

let rec spec = function
  | Int i -> i
  | Plus (a, b) -> spec a + spec b
  | Minus (a, b) -> spec a - spec b
;;

let rec target = function
  | NInt i -> [%synt f0] i
  | NPlus (s, a, b) -> [%synt f1] s
  | NMinus (s, a, b) -> [%synt f2] s
  [@@requires is_memo_psum]
;;
