open Base
open Term
open Option.Let_syntax

let get_id_const (op : Binop.t) : term option =
  match op with
  | Plus -> Some (mk_const (Constant.CInt 0))
  | Times -> Some (mk_const (Constant.CInt 1))
  | Min -> Some (mk_const (Constant.CInt Int.max_value))
  | Max -> Some (mk_const (Constant.CInt Int.min_value))
  | And -> Some (mk_const Constant.CTrue)
  | Or -> Some (mk_const Constant.CFalse)
  | _ -> None

let get_ty_const (typ : RType.t) : term =
  RType.(
    match typ with
    | TInt -> mk_const (Constant.of_int 0)
    | TBool -> mk_const (Constant.of_int 1)
    | _ -> mk_const (Constant.of_int 0))

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

(** Set of ints = set of variables. Module is only meant to avoid (Set.... (module Int)) everywhere. *)
module IS = struct
  type t = Set.M(Int).t

  type elt = int

  let empty = Set.empty (module Int)

  let singleton i = Set.singleton (module Int) i

  let of_list l = Set.of_list (module Int) l

  let ( + ) = Set.union

  let ( - ) = Set.diff

  let ( ^ ) = Set.inter

  let ( ?. ) x = Set.is_empty x

  let ( ~$ ) x = singleton x

  let pp (f : Formatter.t) (s : t) : unit = Fmt.(braces (list ~sep:comma int)) f (Set.elements s)
end

(** An expression is a term without let-bindings or function values.  *)
module Expression = struct
  let box_id : int ref = ref 0

  let new_box_id () =
    let i = !box_id in
    Int.incr box_id;
    i

  type t =
    | ETrue
    | EFalse
    | EInt of int
    | EVar of int
    | EBox of int
    | ETup of t list
    | EIte of t * t * t
    | EData of string * t list
    | EAssoc of Binop.t * t list
    | EBin of Binop.t * t * t
    | EUn of Unop.t * t

  let _VARS : (int, variable) Hashtbl.t = Hashtbl.create (module Int)

  let register_var (v : variable) = Hashtbl.set _VARS ~key:v.vid ~data:v

  let get_var (id : int) = Hashtbl.find _VARS id

  let pp_ivar (f : Formatter.t) (vid : int) : unit =
    Fmt.(
      match get_var vid with
      | Some v -> pf f "%a" (styled (`Fg `Green) (styled `Italic Variable.pp)) v
      | None -> pf f "?%a" (styled (`Fg `Green) (styled `Italic int)) vid)

  let rec pp (f : Formatter.t) (expr : t) : unit =
    Fmt.(
      match expr with
      | ETrue -> pf f "#t"
      | EFalse -> pf f "#f"
      | EInt i -> pf f "%i" i
      | EVar i -> pp_ivar f i
      | EBox i -> pf f ":%a" (styled `Faint int) i
      | ETup tl -> pf f "@[(%a)@]" (list ~sep:comma pp) tl
      | EIte (a, b, c) -> pf f "@[if@;%a then@ %a@ else %a@]" pp a pp b pp c
      | EData (c, tl) -> pf f "@[%s(%a)@]" c (list ~sep:comma pp) tl
      | EAssoc (op, args) ->
          pf f "@[(%a %a)@]" (styled (`Fg `Yellow) Binop.pp) op (list ~sep:sp pp) args
      | EBin (op, a, b) -> pf f "@[(%a@ %a %a)@]" (styled (`Fg `Yellow) Binop.pp) op pp a pp b
      | EUn (op, a) -> pf f "@[(%a %a)@]" Unop.pp op pp a)

  (* Simple equality *)
  let equal : t -> t -> bool = Poly.equal

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

  let reduce ~(case : (t -> 'a) -> t -> 'a option) ~(join : 'a -> 'a -> 'a) ~(init : 'a) (e : t) =
    let rec aux e =
      match case aux e with
      | Some c -> c
      | None -> (
          match e with
          | ETrue | EFalse | EInt _ | EVar _ | EBox _ -> init
          | EIte (a, b, c) -> join (aux a) (join (aux b) (aux c))
          | EAssoc (_, tl) | EData (_, tl) | ETup tl ->
              List.fold ~init ~f:(fun a e' -> join (aux e') a) tl
          | EBin (_, a, b) -> join (aux a) (aux b)
          | EUn (_, a) -> aux a)
    in
    aux e

  let transform (case : (t -> t) -> t -> t option) (e : t) =
    let rec aux e =
      match case aux e with
      | Some c -> c
      | None -> (
          match e with
          | ETrue | EFalse | EInt _ | EVar _ | EBox _ -> e
          | EIte (a, b, c) -> mk_e_ite (aux a) (aux b) (aux c)
          | EAssoc (op, tl) -> mk_e_assoc op (List.map ~f:aux tl)
          | EData (c, tl) -> mk_e_data c (List.map ~f:aux tl)
          | ETup tl -> mk_e_tup (List.map ~f:aux tl)
          | EBin (op, a, b) -> mk_e_bin op (aux a) (aux b)
          | EUn (op, a) -> mk_e_un op (aux a))
    in
    aux e

  let free_variables (e : t) =
    IS.(
      reduce ~join:( + ) ~init:empty
        ~case:(fun _ e -> match e with EVar i -> Some ~$i | _ -> None)
        e)

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
      | EBox i ->
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

  (** Matching subexpressions (up to rewriting) *)
  let match_as_subexpr (e : t) ~(of_ : t) : (int * t) option =
    let bid = ref None in
    let transformer _ e0 =
      if equal e e0 then (
        match !bid with
        | Some i -> Some (EBox i)
        | None ->
            let i = new_box_id () in
            bid := Some i;
            Some (EBox i))
      else None
    in
    let e' = transform transformer of_ in
    match !bid with Some id -> Some (id, e') | None -> None

  let get_id_const (op : Binop.t) : t option =
    match op with
    | Plus -> Some (mk_e_int 0)
    | Times -> Some (mk_e_int 1)
    | Min -> Some (mk_e_int Int.max_value)
    | Max -> Some (mk_e_int Int.min_value)
    | And -> Some mk_e_true
    | Or -> Some mk_e_false
    | _ -> None

  let get_ty_const (typ : RType.t) : t =
    RType.(match typ with TInt -> mk_e_int 0 | TBool -> mk_e_true | _ -> mk_e_int 0)
end
