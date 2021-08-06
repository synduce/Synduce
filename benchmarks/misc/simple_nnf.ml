type formula =
  | FLit of bool
  | FAnd of formula * formula
  | FOr of formula * formula
  | FNot of formula

let rec eval = function
  | FLit b -> b
  | FAnd (x, y) -> eval x && eval y
  | FOr (x, y) -> eval x || eval y
  | FNot x -> not (eval x)

type nnf_formula =
  | NFNegLit of bool
  | NFLit of bool
  | NFAnd of nnf_formula * nnf_formula
  | NFOr of nnf_formula * nnf_formula

let rec repr = function
  | NFLit b -> FLit b
  | NFNegLit b -> FNot (FLit b)
  | NFAnd (x, y) -> FAnd (repr x, repr y)
  | NFOr (x, y) -> FOr (repr x, repr y)

let rec eval2 = function
  | NFLit b -> [%synt elit] b
  | NFNegLit b -> [%synt eneglit] b
  | NFAnd (x, y) -> [%synt eand] (eval2 x) (eval2 y)
  | NFOr (x, y) -> [%synt eor] (eval2 x) (eval2 y)
;;

assert (eval2 = repr @@ eval)
