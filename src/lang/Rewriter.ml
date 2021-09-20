open Base
open Term
open Option.Let_syntax
open Utils

let get_id_const (op : Binop.t) : term option =
  match op with
  | Plus -> Some (mk_const (Constant.CInt 0))
  | Times -> Some (mk_const (Constant.CInt 1))
  | Min -> Some (mk_const (Constant.CInt Int.max_value))
  | Max -> Some (mk_const (Constant.CInt Int.min_value))
  | And -> Some (mk_const Constant.CTrue)
  | Or -> Some (mk_const Constant.CFalse)
  | _ -> None

let is_id_int_const (op : Binop.t) (i : int) : bool =
  match (op, i) with Plus, 0 | Div, 1 | Minus, 0 | Times, 1 -> true | _ -> false

let is_null_int_const (op : Binop.t) (i : int) : bool =
  match (op, i) with Times, 0 -> true | _ -> false

let is_null_bool_const (op : Binop.t) (b : bool) : bool =
  match (op, b) with And, false | Or, true -> true | _ -> false

let is_and op = Binop.(match op with And -> true | _ -> false)

let is_or op = Binop.(match op with Or -> true | _ -> false)

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
    Operator.
      [
        (Binary Plus, Binary Max);
        (Binary Times, Binary Plus);
        (Binary Plus, Binary Min);
        (Binary And, Binary Or);
      ]

(** For example, `is_left_distrib Plus Max` is true. *)
let is_left_distrib op1 op2 =
  match Map.find _left_distrib op1 with
  | Some binops -> Binop.(List.mem binops ~equal op2)
  | None -> false

let get_distributing op =
  Operator.(
    match op with
    | Binary Plus -> Binary Times
    | Binary (Min | Max) -> Binary Plus
    | Binary Or -> Binary And
    | _ ->
        Log.(error (wrap1 "no distributing op for %a" Operator.pp op));
        failwith "Error in rewriting.")

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
let is_commutative op = Set.mem _commut op

let concrete_int_op (op : Operator.t) =
  Operator.(
    match op with
    | Binary Plus -> Some ( + )
    | Binary Times -> Some ( * )
    | Binary Minus -> Some ( - )
    | Binary Div -> Some ( * )
    | _ -> None)

(** Set of ints = set of variables. Module is only meant to avoid (Set.... (module Int)) everywhere. *)
module IS = struct
  type t = Set.M(Int).t

  type elt = int

  let empty = Set.empty (module Int)

  let singleton i = Set.singleton (module Int) i

  let of_list l = Set.of_list (module Int) l

  let ( + ) = Set.union

  let ( - ) = Set.diff

  (** Set intersection. *)
  let ( ^ ) = Set.inter

  (** Checks is a set is empty.*)
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
    | EOp of Operator.t * t list

  let _VARS : (int, variable) Hashtbl.t = Hashtbl.create (module Int)

  let register_var (v : variable) = Hashtbl.set _VARS ~key:v.vid ~data:v

  let get_var (id : int) = Hashtbl.find _VARS id

  let pp_ivar (f : Formatter.t) (vid : int) : unit =
    Fmt.(
      match get_var vid with
      | Some v -> pf f "%a" (styled (`Fg `Green) (styled `Italic Variable.pp)) v
      | None -> pf f "?%a" (styled (`Fg `Green) (styled `Italic int)) vid)

  let pp_ivarset (f : Formatter.t) (vis : IS.t) : unit =
    Fmt.(braces (list ~sep:comma pp_ivar)) f (Set.elements vis)

  let rec pp (f : Formatter.t) (expr : t) : unit =
    Fmt.(
      match expr with
      | ETrue -> pf f "#t"
      | EFalse -> pf f "#f"
      | EInt i -> pf f "%i" i
      | EVar i -> pp_ivar f i
      | EBox i -> pf f ":%a" (styled `Faint int) i
      | ETup tl -> pf f "@[(%a)@]" (list ~sep:comma pp) tl
      | EIte (a, b, c) ->
          pf f "@[%a@;%a %a@ %a@ %a %a@]"
            (styled `Faint (styled `Bold string))
            "if" pp a
            (styled `Faint (styled `Bold string))
            "then" pp b
            (styled `Faint (styled `Bold string))
            "else" pp c
      | EData (c, tl) -> pf f "@[%s(%a)@]" c (list ~sep:comma pp) tl
      | EOp (op, args) -> (
          match args with
          | [ a ] -> pf f "@[(%a %a)@]" Operator.pp op pp a
          | [ a; b ] -> (
              match op with
              | Binary Max | Binary Min ->
                  pf f "@[%a(%a, %a)@]" (styled (`Fg `Yellow) Operator.pp) op pp a pp b
              | _ -> pf f "@[(%a %a@ %a)@]" pp a (styled (`Fg `Yellow) Operator.pp) op pp b)
          | _ -> pf f "@[(%a %a)@]" (styled (`Fg `Yellow) Operator.pp) op (list ~sep:sp pp) args))

  (* Simple equality *)

  let equal : t -> t -> bool = Poly.equal

  (* Expression construction. *)
  let mk_e_true = ETrue

  let mk_e_false = EFalse

  let mk_e_int i = EInt i

  let mk_e_bool b = if b then mk_e_true else mk_e_false

  let mk_e_var id = EVar id

  let mk_e_tup tl = ETup tl

  let mk_e_assoc op l =
    match l with
    | [] -> failwith "Creation of operation with empty args"
    | [ a ] -> a
    | _ -> EOp (op, l)

  let mk_e_ite a b c = EIte (a, b, c)

  let mk_e_data c l = EData (c, l)

  let mk_e_bin b e1 e2 = EOp (Binary b, [ e1; e2 ])

  let mk_e_un u e = EOp (Unary u, [ e ])

  module Op = struct
    let ( + ) = mk_e_bin Binop.Plus

    let ( - ) = mk_e_bin Binop.Minus

    let ( * ) = mk_e_bin Binop.Times

    let ( / ) = mk_e_bin Binop.Div

    let ( && ) = mk_e_bin Binop.And

    let ( || ) = mk_e_bin Binop.Or

    let max = mk_e_bin Binop.Max

    let min = mk_e_bin Binop.Min

    let not e1 = mk_e_un Unop.Not e1

    let int i = mk_e_int i

    let var i = mk_e_var i

    let ( ~? ) a = mk_e_ite a
  end

  (* Expression reduction / transformation. *)

  let reduce ~(case : (t -> 'a) -> t -> 'a option) ~(join : 'a -> 'a -> 'a) ~(init : 'a) (e : t) =
    let rec aux e =
      match case aux e with
      | Some c -> c
      | None -> (
          match e with
          | ETrue | EFalse | EInt _ | EVar _ | EBox _ -> init
          | EIte (a, b, c) -> join (aux a) (join (aux b) (aux c))
          | EOp (_, tl) | EData (_, tl) | ETup tl ->
              List.fold ~init ~f:(fun a e' -> join (aux e') a) tl)
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
          | EOp (op, tl) -> mk_e_assoc op (List.map ~f:aux tl)
          | EData (c, tl) -> mk_e_data c (List.map ~f:aux tl)
          | ETup tl -> mk_e_tup (List.map ~f:aux tl))
    in
    aux e

  let rewrite_until_stable (rule : t -> t) (e : t) : t =
    let rec aux i e0 =
      if i > !Config.rewrite_limit then e0
      else
        let e0' = rule e0 in
        if equal e0' e0 then e0 else aux (i + 1) e0'
    in
    aux 0 e

  (*  *)
  let expr_size (e : t) : int = reduce ~case:(fun _ _ -> None) ~init:1 ~join:( + ) e

  let expr_size_compare (e1 : t) (e2 : t) = compare (expr_size e1) (expr_size e2)

  let compare a b =
    let rec aux a b =
      match (a, b) with
      | EFalse, EFalse | ETrue, ETrue -> 0
      | EFalse, ETrue -> -1
      | ETrue, EFalse -> 1
      | EInt a, EInt b | EBox a, EBox b | EVar a, EVar b -> compare a b
      | EIte (c, a, b), EIte (c', a', b') -> List.compare aux [ c; a; b ] [ c'; a'; b' ]
      | EData (c, args), EData (c', args') ->
          let z = String.compare c c' in
          if z = 0 then List.compare aux args args' else z
      | EOp (op, args), EOp (op', args') ->
          let z = Operator.compare op op' in
          if z = 0 then List.compare aux args args' else z
      | ETup tl, ETup tl' -> List.compare aux tl tl'
      | _ ->
          let z = expr_size_compare a b in
          if z = 0 then Poly.compare a b else z
    in
    aux a b

  let equal a b = compare a b = 0

  let free_variables (e : t) =
    IS.(
      reduce ~join:( + ) ~init:empty
        ~case:(fun _ e -> match e with EVar i -> Some ~$i | _ -> None)
        e)

  (** [alpha_equal e1 e2] returns true if [equal e1 e2] up to variable renaming. *)
  let alpha_equal (e1 : t) (e2 : t) : bool =
    let _ = (e1, e2) in
    failwith "TODO: Not implemented."

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
            | Plus -> mk_e_assoc (Binary Plus) [ f t1; f t2 ]
            | Times -> mk_e_assoc (Binary Times) [ f t1; f t2 ]
            | Div -> mk_e_assoc (Binary Times) [ f t1; mk_e_un Unop.Inv (f t2) ]
            | And -> mk_e_assoc (Binary And) [ f t1; f t2 ]
            | Or -> mk_e_assoc (Binary Or) [ f t1; f t2 ]
            | Min -> mk_e_assoc (Binary Min) [ f t1; f t2 ]
            | Max -> mk_e_assoc (Binary Max) [ f t1; f t2 ]
            | Gt -> mk_e_bin Gt (f t1) (f t2)
            | Ge -> mk_e_bin Ge (f t1) (f t2)
            | Lt -> mk_e_bin Gt (f t2) (f t1)
            | Le -> mk_e_bin Ge (f t2) (f t1)
            | Eq -> mk_e_bin Eq (f t1) (f t2)
            | Mod -> mk_e_bin Mod (f t1) (f t2)
            | Minus -> mk_e_assoc (Binary Plus) [ f t1; mk_e_un Neg (f t2) ]))
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
      | ETrue -> Some (Terms.bool true)
      | EFalse -> Some (Terms.bool false)
      | EInt i -> Some (Terms.int i)
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
      | EOp (op, tl) -> (
          match (tl, op) with
          | [ a ], Unary op ->
              let%map a' = f a in
              mk_un op a'
          | [ a; b ], Binary op ->
              let%map a' = f a and b' = f b in
              mk_bin op a' b'
          | _ -> (
              Binop.(
                match op with
                | Binary Plus ->
                    Option.bind ~f:(mk_assoc_with_id Binop.Plus) (Option.all (List.map ~f tl))
                | Binary Times ->
                    Option.bind ~f:(mk_assoc_with_id Binop.Times) (Option.all (List.map ~f tl))
                | Binary Max ->
                    Option.bind ~f:(mk_assoc_with_id Binop.Max) (Option.all (List.map ~f tl))
                | Binary Min ->
                    Option.bind ~f:(mk_assoc_with_id Binop.Min) (Option.all (List.map ~f tl))
                | Binary And ->
                    Option.bind ~f:(mk_assoc_with_id Binop.And) (Option.all (List.map ~f tl))
                | Binary Or ->
                    Option.bind ~f:(mk_assoc_with_id Binop.Or) (Option.all (List.map ~f tl))
                | _ -> None)))
    in

    f e

  (** Evaluation *)
  let simplify (e : t) : t =
    let ieval op f args =
      let symbs, maybe_concrete =
        List.fold args ~init:([], None) ~f:(fun (symbs, concrete) arg ->
            match arg with
            | EInt i -> (
                match concrete with Some i' -> (symbs, Some (f i i')) | None -> (symbs, Some i))
            | _ -> (symbs @ [ arg ], concrete))
      in
      let args' =
        match maybe_concrete with
        | Some i -> if is_null_int_const op i then [ mk_e_int i ] else symbs @ [ mk_e_int i ]
        | None -> symbs
      in
      match
        (* Filter out identity elements. *)
        List.filter
          ~f:(fun x -> match x with EInt i -> not (is_id_int_const op i) | _ -> true)
          args'
      with
      | [] -> [ List.hd_exn args' ]
      | _ as l -> l
    in
    let beval op f args =
      let args = List.dedup_and_sort ~compare args in
      let symbs, maybe_concrete =
        List.fold args ~init:([], None) ~f:(fun (symbs, concrete) arg ->
            match arg with
            | ETrue -> (
                match concrete with
                | Some i' -> (symbs, Some (f true i'))
                | None -> (symbs, Some true))
            | EFalse -> (
                match concrete with
                | Some i' -> (symbs, Some (f false i'))
                | None -> (symbs, Some false))
            | _ -> (symbs @ [ arg ], concrete))
      in
      let args' =
        match maybe_concrete with
        | Some b -> if is_null_bool_const op b then [ mk_e_bool b ] else symbs @ [ mk_e_bool b ]
        | None -> symbs
      in
      match
        List.filter
          ~f:(fun x -> match x with ETrue -> is_and op | EFalse -> is_or op | _ -> true)
          args'
      with
      | [] -> [ List.hd_exn args' ]
      | _ as l -> l
    in
    let tr f e =
      match e with
      | EIte (c, a, b) ->
          Some (match f c with ETrue -> f a | EFalse -> f b | c' -> EIte (c', f a, f b))
      | EOp (op, args) -> (
          match List.map ~f args with
          | [ x ] -> Some x
          | args -> (
              match op with
              | Binary Plus -> Some (mk_e_assoc op (ieval Binop.Plus ( + ) args))
              | Binary Times -> Some (mk_e_assoc op (ieval Binop.Times ( + ) args))
              | Binary Max -> Some (mk_e_assoc op (ieval Binop.Max ( + ) args))
              | Binary Min -> Some (mk_e_assoc op (ieval Binop.Min ( + ) args))
              | Binary And -> Some (mk_e_assoc op (beval Binop.And ( && ) args))
              | Binary Or -> Some (mk_e_assoc op (beval Binop.Or ( || ) args))
              | _ -> Some (mk_e_assoc op args)))
      | _ -> None
    in
    rewrite_until_stable (transform tr) e

  (** Normalizing *)
  let normalize (e : t) : t =
    let coll op args =
      mk_e_assoc op
        (List.concat_map args ~f:(fun x ->
             match x with
             | EOp (op', args') -> if Operator.(equal op op') then args' else [ x ]
             | _ -> [ x ]))
    in
    let rule e0 =
      let g f e' =
        match e' with EOp (op, args) -> Some (coll op (List.map ~f args)) | _ -> None
      in
      simplify (transform g e0)
    in
    rewrite_until_stable rule e

  (** Get the identity element of a given operator.*)
  let get_id_const (op : Operator.t) : t option =
    match op with
    | Binary Plus | Unary Neg | Binary Minus -> Some (mk_e_int 0)
    | Binary Times -> Some (mk_e_int 1)
    | Binary Min -> Some (mk_e_int Int.max_value)
    | Binary Max -> Some (mk_e_int Int.min_value)
    | Binary And -> Some mk_e_true
    | Binary Or -> Some mk_e_false
    | _ -> None

  (** Get a constant of a given type (implemented fo TInt and TBool). *)
  let get_ty_const (typ : RType.t) : t =
    RType.(match typ with TInt -> mk_e_int 0 | TBool -> mk_e_true | _ -> mk_e_int 0)
end

open Expression

let collect_common_factors (op : Operator.t) (args : t list) : (int * Operator.t option * t) list =
  let add_fac op_opt factors e =
    let added, factors' =
      (* Find a matching factor - if found, add it to existing factors. *)
      List.fold factors ~init:(false, [])
        ~f:(fun (added, factors) (fac_count, fac_op_option, fac_expr) ->
          if equal fac_expr e then
            match (op_opt, fac_op_option) with
            | Some op, Some fo when Operator.equal op fo ->
                (true, (fac_count + 1, Some fo, e) :: factors)
            | None, Some fo -> (added, (fac_count + 1, Some fo, e) :: factors)
            | Some op, None -> (true, (fac_count + 1, Some op, e) :: factors)
            | None, None -> (true, (fac_count + 1, None, e) :: factors)
            | _ -> (added, (fac_count, fac_op_option, fac_expr) :: factors)
          else
            (* List of factors unchanged. *)
            (added, (fac_count, fac_op_option, fac_expr) :: factors))
    in
    if added then factors' else (1, op_opt, e) :: factors'
  in
  let f factors arg =
    match arg with
    | EOp (op', args') when is_left_distrib op' op ->
        List.fold ~init:factors ~f:(add_fac (Some op')) args'
    | _ -> List.fold ~init:factors ~f:(add_fac None) [ arg ]
  in
  List.fold ~f ~init:[] args

let best_factor (factors : (int * Operator.t option * t) list) : (Operator.t option * t) option =
  Fmt.(pf stdout "%i factors:@." (List.length factors));
  List.iter ~f:Fmt.(fun (_, _, e) -> pf stdout "%a@." pp e) factors;
  match List.max_elt ~compare:(fun (i1, _, _) (i2, _, _) -> Int.compare i1 i2) factors with
  | Some (i, op, e) -> if i >= 2 then Some (op, e) else None
  | None -> None

let apply_factor (op : Operator.t) (args : t list) (fac_op : Operator.t option) (fac_expr : t) =
  let fac_op =
    try Option.value fac_op ~default:(get_distributing op)
    with Failure s ->
      Log.(error (wrap2 "(%a %a)" Operator.pp op Fmt.(list ~sep:sp pp) args));
      Log.(error (wrap1 "(fac_expr: %a" pp fac_expr));
      failwith s
  in
  let args_with_fac, args_no_fac =
    let f arg =
      match arg with
      | EOp (op', args') when Operator.equal op' fac_op -> (
          let facs, rest = List.partition_tf ~f:(fun e -> equal e fac_expr) args' in
          match facs with
          | [] -> Either.Second arg
          | _ :: tl -> Either.First (mk_e_assoc op' (tl @ rest)))
      | _ ->
          if equal arg fac_expr then
            match get_id_const fac_op with Some e -> Either.First e | _ -> Either.Second arg
          else Either.Second arg
    in
    List.partition_map ~f args
  in
  match args_with_fac with
  | [] -> normalize (mk_e_assoc op args_no_fac)
  | _ ->
      let factorized = mk_e_assoc fac_op [ fac_expr; mk_e_assoc op args_with_fac ] in
      normalize (mk_e_assoc op (factorized :: args_no_fac))

(** Factorize a term using distributivity rules. *)
let factorize (e : t) : t =
  let fac_rule e =
    let case f e =
      match e with
      | EOp (op, args) ->
          let args' = List.map ~f args in
          let factors = collect_common_factors op args' in
          Option.map (best_factor factors) ~f:(fun (fac_op, fac_expr) ->
              let e = apply_factor op args' fac_op fac_expr in
              e)
      | _ -> None
    in
    normalize (transform case e)
  in
  rewrite_until_stable fac_rule (normalize e)

let distrib (op1 : Operator.t) (args : t list) : t =
  let f hd tl =
    mk_e_assoc op1
      (List.fold ~init:[ hd ] tl ~f:(fun acc e' ->
           match e' with
           | EOp (op2, args) ->
               if is_left_distrib op1 op2 then
                 [ mk_e_assoc op2 (List.map args ~f:(fun arg -> mk_e_assoc op1 (acc @ [ arg ]))) ]
               else acc @ [ e' ]
           | _ -> acc @ [ e' ]))
  in
  let args = if is_commutative op1 then List.sort ~compare args else args in
  match args with [ a ] -> a | hd :: tl -> f hd tl | _ -> failwith "Empty application."

(** Expand a term by applying distributivity rules.*)
let expand (e : t) : t =
  let expand_rule e =
    let case f e =
      match e with EOp (op1, args) -> Some (distrib op1 (List.map ~f args)) | _ -> None
    in
    normalize (transform case e)
  in
  rewrite_until_stable expand_rule e

(** Rewrite rule out of a lemma. *)
let rewrite_with_lemma (lemma : t) : t -> t list =
  let conjs = match expand lemma with EOp (Binary And, args) -> args | lemma' -> [ lemma' ] in
  let dyn_rules =
    let r e =
      let open Operator in
      match e with
      (* a >= x : a -> max a x *)
      | EOp (Binary Ge, [ a; EInt x ]) -> [ (a, mk_e_assoc (Binary Max) [ a; EInt x ]) ]
      (* a <= x : a -> min a x *)
      | EOp (Binary Le, [ a; EInt x ]) -> [ (a, mk_e_assoc (Binary Min) [ a; EInt x ]) ]
      | _ -> []
    in
    List.concat_map ~f:r conjs
  in
  let apply_dyn_rule e =
    List.map dyn_rules ~f:(fun (lhs, rhs) ->
        let e' = transform (fun _ e -> if equal lhs e then Some rhs else None) e in
        e')
  in
  apply_dyn_rule

(** Matching subexpressions (up to rewriting) *)
let match_as_subexpr ?(lemma = None) (sube : t) ~(of_ : t) : (int * t) option =
  (* Expand expressions. *)
  let of_ = expand of_ and sube = expand sube in
  (* Use lemmas to rewrite expressions.  *)
  let of_choices =
    let choices_from_lemma_r =
      Option.value
        (Option.map ~f:(fun l -> of_ :: List.map ~f:expand (rewrite_with_lemma l of_)) lemma)
        ~default:[ of_ ]
    in
    List.remove_consecutive_duplicates ~equal choices_from_lemma_r
  in
  let bids = Hashtbl.create (module Int) in
  let transformer ~i _ e0 =
    if equal sube e0 then (
      match Hashtbl.find bids i with
      | Some bid -> Some (EBox bid)
      | None ->
          let bid = new_box_id () in
          Hashtbl.set bids ~key:i ~data:bid;
          Some (EBox bid))
    else
      match (e0, sube) with
      | EOp (op, args), EOp (subop, subargs) when Operator.equal op subop ->
          let subargs', rest = List.partition_tf ~f:(List.mem ~equal subargs) args in
          if List.length subargs' = List.length subargs then (
            match Hashtbl.find bids i with
            | Some bid -> Some (mk_e_assoc op (EBox bid :: rest))
            | None ->
                let bid = new_box_id () in
                Hashtbl.set bids ~key:i ~data:bid;
                Some (mk_e_assoc op (EBox bid :: rest)))
          else None
      | _ -> None
  in
  let e_choices = List.mapi ~f:(fun i x -> transform (transformer ~i) x) of_choices in
  let results =
    List.mapi e_choices ~f:(fun i ec ->
        match Hashtbl.find bids i with Some id -> Some (id, ec) | None -> None)
  in
  match List.filter_opt results with hd :: _ -> Some hd | _ -> None
