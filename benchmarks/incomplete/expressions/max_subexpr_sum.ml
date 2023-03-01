(** @synduce -s 2 -NB -n 20 --no-lifting *)

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
  | NPlus (s, a, b) -> s >= sum a + sum b && is_memo_psum a && is_memo_psum b
  | NMinus (s, a, b) -> s >= sum a - sum b && is_memo_psum a && is_memo_psum b

and sum = function
  | NInt i -> i
  | NPlus (s, a, b) -> sum a + sum b
  | NMinus (s, a, b) -> sum a - sum b
;;

let rec spec = function
  | Int i -> i, i
  | Plus (a, b) ->
    let asum, am = spec a in
    let bsum, bm = spec b in
    asum + bsum, max (asum + bsum) (max am bm)
  | Minus (a, b) ->
    let asum, am = spec a in
    let bsum, bm = spec b in
    asum - bsum, max (asum - bsum) (max am bm)
;;

let rec target = function
  | NInt i -> [%synt f0]
  | NPlus (s, a, b) -> [%synt f1]
  | NMinus (s, a, b) -> [%synt f2]
;;
