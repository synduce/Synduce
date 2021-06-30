open Base
open Term
open Option.Let_syntax

let get_id_const (op : Binop.t) : term option =
  match op with
  | Plus -> Some (mk_const (Constant.CInt 0))
  | Times -> Some (mk_const (Constant.CInt 0))
  | Min -> Some (mk_const (Constant.CInt Int.max_value))
  | Max -> Some (mk_const (Constant.CInt Int.min_value))
  | And -> Some (mk_const Constant.CTrue)
  | Or -> Some (mk_const Constant.CFalse)
  | _ -> None

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

let mk_assoc_with_id (op : Binop.t) (tl : term list) : term option =
  let rec aux t rest =
    match rest with hd :: tl -> aux (mk_bin ~pos:hd.tpos ~typ:(Some hd.ttyp) op t hd) tl | [] -> t
  in
  match tl with [] -> get_id_const op | [ x ] -> Some x | hd :: tl -> Some (aux hd tl)

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

(** An expression is a term without let-bindings or function values.  *)
module Expression = struct
  type t =
    | ETrue
    | EFalse
    | EInt of int
    | EVar of int
    | ETup of t list
    | EIte of t * t * t
    | EData of string * t list
    | EAssoc of Binop.t * t list
    | EBin of Binop.t * t * t
    | EUn of Unop.t * t

  (* Simple equality *)
  let equal = Poly.equal

  (* Expression construction. *)
  let mk_e_true = ETrue

  let mk_e_false = EFalse

  let mk_e_int i = EInt i

  let mk_e_var id = EVar id

  let mk_e_tup tl = ETup tl

  let mk_e_assoc op l = EAssoc (op, l)

  let mk_e_ite a b c = EIte (a, b, c)

  let mk_e_data c l = EData (c, l)

  let mk_e_bin b e1 e2 = EBin (b, e1, e2)

  let mk_e_un u e = EUn (u, e)

  let _VARS : (int, variable) Hashtbl.t = Hashtbl.create (module Int)

  let register_var (v : variable) = Hashtbl.set _VARS ~key:v.vid ~data:v

  let get_var (id : int) = Hashtbl.find _VARS id

  let of_term t0 : t option =
    let rec f t =
      match t.tkind with
      | TBox t -> f t
      | TConst c -> (
          Constant.(match c with CInt i -> mk_e_int i | CTrue -> mk_e_true | CFalse -> mk_e_false))
      | TVar v ->
          register_var v;
          mk_e_var v.vid
      | TData (c, tl) -> mk_e_data c (List.map ~f tl)
      | TTup tl -> mk_e_tup (List.map ~f tl)
      | TIte (c, tt, tf) -> mk_e_ite (f c) (f tt) (f tf)
      | TBin (op, t1, t2) -> (
          Binop.(
            match op with
            | Plus -> mk_e_assoc Plus [ f t1; f t2 ]
            | Times -> mk_e_assoc Times [ f t1; f t2 ]
            | Div -> mk_e_assoc Times [ f t1; mk_e_un Unop.Inv (f t2) ]
            | And -> mk_e_assoc And [ f t1; f t2 ]
            | Or -> mk_e_assoc Or [ f t1; f t2 ]
            | Min -> mk_e_assoc Min [ f t1; f t2 ]
            | Max -> mk_e_assoc Max [ f t1; f t2 ]
            | Gt -> mk_e_bin Gt (f t1) (f t2)
            | Ge -> mk_e_bin Ge (f t1) (f t2)
            | Lt -> mk_e_bin Gt (f t2) (f t1)
            | Le -> mk_e_bin Ge (f t2) (f t1)
            | Eq -> mk_e_bin Eq (f t1) (f t2)
            | Mod -> mk_e_bin Mod (f t1) (f t2)
            | Minus -> mk_e_assoc Plus [ f t1; mk_e_un Neg (f t2) ]))
      | TUn (op, t) -> (
          Unop.(
            match op with
            | Inv -> mk_e_un Inv (f t)
            | Neg -> mk_e_un Neg (f t)
            | Not -> mk_e_un Not (f t)
            | Abs -> mk_e_un Abs (f t)))
      | TMatch _ | TApp _ | TFun _ | TSel _ ->
          raise_s (Sexp.Atom "Expressions only for fully reduced terms.")
    in
    try Some (f t0) with _ -> None

  let to_term e : term option =
    let rec f e =
      match e with
      | ETrue -> Some (mk_const Constant.CTrue)
      | EFalse -> Some (mk_const Constant.CFalse)
      | EInt i -> Some (mk_const (Constant.CInt i))
      | EVar i ->
          let%map v = get_var i in
          mk_var v
      | ETup tl -> Option.map ~f:mk_tup (Option.all (List.map ~f tl))
      | EIte (c, tt, tf) ->
          let%map c' = f c and tt' = f tt and tf' = f tf in
          mk_ite c' tt' tf'
      | EData (c, tl) -> Option.map ~f:(mk_data c) (Option.all (List.map ~f tl))
      | EAssoc (op, tl) -> (
          Binop.(
            match op with
            | Plus -> Option.bind ~f:(mk_assoc_with_id Binop.Plus) (Option.all (List.map ~f tl))
            | Times -> Option.bind ~f:(mk_assoc_with_id Binop.Times) (Option.all (List.map ~f tl))
            | Max -> Option.bind ~f:(mk_assoc_with_id Binop.Max) (Option.all (List.map ~f tl))
            | Min -> Option.bind ~f:(mk_assoc_with_id Binop.Min) (Option.all (List.map ~f tl))
            | And -> Option.bind ~f:(mk_assoc_with_id Binop.And) (Option.all (List.map ~f tl))
            | Or -> Option.bind ~f:(mk_assoc_with_id Binop.Or) (Option.all (List.map ~f tl))
            | _ -> None))
      | EUn (op, a) ->
          let%map a' = f a in
          mk_un op a'
      | EBin (op, a, b) ->
          let%map a' = f a and b' = f b in
          mk_bin op a' b'
    in
    f e
end
