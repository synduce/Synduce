open Base
open Term

(**
  Left distributive operators: for each pair op1, op2 it means that:
  a op1 (b op2 c) = (a op1 b) op2 (a op1 c)
*)
let _left_distrib =
  Map.of_alist_multi
    (module Operator)
    Binop.[ (Binary Plus, Max); (Binary Times, Plus); (Binary Plus, Min); (Binary And, Or) ]

(** For example, `is_left_distrib Plus Max` is true. *)
let is_left_distrib op1 op2 =
  match Map.find _left_distrib (Binary op1) with
  | Some binops -> Binop.(List.mem binops ~equal op2)
  | None -> false

(**
  Right distributive operators: for each pair op1, op2 it means that:
  (b op2 c) op1 a = (b op1 a) op2 (c op1 a)
*)
let _right_distrib =
  Map.of_alist_multi
    (module Operator)
    Binop.
      [
        (Binary Plus, Max);
        (Binary Times, Plus);
        (Binary Div, Plus);
        (Binary Plus, Min);
        (Binary And, Or);
      ]

(** For example, `is_right_distrib Plus Max` is true. *)
let is_right_distrib op1 op2 =
  match Map.find _right_distrib (Binary op1) with
  | Some binops -> Binop.(List.mem binops ~equal op2)
  | None -> false

(**
  Associative operators.
*)
let _assoc = OpSet.of_list [ Binary Plus; Binary Times; Binary Min; Binary Max ]

(**
  For example, `is_assoc Plus` is true.
*)
let is_assoc op = Set.mem _assoc (Binary op)

(**
  Commutative operators.
*)
let _commut =
  OpSet.of_list
    [ Binary Plus; Binary Times; Binary Min; Binary Max; Binary Eq; Binary And; Binary Or ]

(**
  For example, `is_commutative Plus` is true.
*)
let is_commutative op = Set.mem _commut (Binary op)

(**
  Convert an expression to enf:
  - no binary Minus : a - b --> a + (-b)
  - no Lt, Le : a < b --> b > a and a <= b --> b >= a
  - unary minus pushed to the leaves.
*)
let to_enf (t0 : term) =
  let r t =
    match t.tkind with
    | TBin (Minus, t1, t2) -> mk_bin Plus t1 (mk_un Neg t2)
    | TBin (Le, t1, t2) -> mk_bin Ge t2 t1
    | TBin (Lt, t1, t2) -> mk_bin Gt t2 t1
    | TUn (Neg, t1) -> (
        match t1.tkind with
        (* - (a + b) --> (-a) + (-b) *)
        | TBin (Plus, a, b) -> mk_bin Plus (mk_un Neg a) (mk_un Neg b)
        (* - (a - b) --> (-a) + b *)
        | TBin (Minus, a, b) -> mk_bin Plus (mk_un Neg a) b
        (* - min(a,b) --> max(-a, -b) *)
        | TBin (Min, a, b) -> mk_bin Max (mk_un Neg a) (mk_un Neg b)
        (* - max(a,b) --> min(-a, -b) *)
        | TBin (Max, a, b) -> mk_bin Min (mk_un Neg a) (mk_un Neg b)
        (* - (a * b) --> -a * -b (or replace * with /)*)
        | TBin (((Times | Div) as op), a, b) -> mk_bin op (mk_un Neg a) b
        | TUn (Neg, a) -> a
        | _ -> t)
    | _ -> t
  in
  rewrite_with r t0

module Expressions = struct
  type t =
    | ETrue
    | EFalse
    | EInt of int
    | EVar of int
    | EPlus of t list
    | ETimes of t list
    | EMax of t list
    | EMin of t list
    | EAnd of t list
    | EOr of t list
    | EDiv of t * t
    | ENot of t
    | EAbs of t
    | EGt of t * t
    | EEq of t * t
    | EGe of t * t
    | EData of string * t list
end
