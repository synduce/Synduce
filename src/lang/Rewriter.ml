open Base
open EProps
open Expression
open Option.Let_syntax
module Operator = Term.Operator

let collect_common_factors (top_op : Operator.t option) (args : t list)
    : (int * Operator.t option * t) list
  =
  let add_fac op_opt factors e =
    let added, factors' =
      (* Find a matching factor - if found, add it to existing factors. *)
      List.fold
        factors
        ~init:(false, [])
        ~f:(fun (added, factors) (fac_count, fac_op_option, fac_expr) ->
          if equal fac_expr e
          then (
            match op_opt, fac_op_option with
            | Some op, Some fo when Operator.equal op fo ->
              true, (fac_count + 1, Some fo, e) :: factors
            | None, Some fo -> added, (fac_count + 1, Some fo, e) :: factors
            | Some op, None -> true, (fac_count + 1, Some op, e) :: factors
            | None, None -> true, (fac_count + 1, None, e) :: factors
            | _ -> added, (fac_count, fac_op_option, fac_expr) :: factors)
          else
            (* List of factors unchanged. *)
            added, (fac_count, fac_op_option, fac_expr) :: factors)
    in
    if added then factors' else (1, op_opt, e) :: factors'
  in
  let f factors arg =
    match top_op with
    | Some top_op ->
      (match arg with
      | EOp (op', args') when is_left_distrib op' top_op ->
        List.fold ~init:factors ~f:(add_fac (Some op')) args'
      | _ -> List.fold ~init:factors ~f:(add_fac None) [ arg ])
    | None ->
      (match arg with
      | EOp (op', args') -> List.fold ~init:factors ~f:(add_fac (Some op')) args'
      | _ -> List.fold ~init:factors ~f:(add_fac None) [ arg ])
  in
  List.fold ~f ~init:[] args
;;

let best_factor (factors : (int * Operator.t option * t) list)
    : (Operator.t option * t) option
  =
  match
    List.max_elt ~compare:(fun (i1, _, _) (i2, _, _) -> Int.compare i1 i2) factors
  with
  | Some (i, op, e) -> if i >= 2 then Some (op, e) else None
  | None -> None
;;

let apply_factor
    (op : Operator.t)
    (args : t list)
    (fac_op : Operator.t option)
    (fac_expr : t)
  =
  let%map fac_op = Option.first_some fac_op (get_distributing op) in
  let args_with_fac, args_no_fac =
    let f arg =
      match arg with
      | EOp (op', args') when Operator.equal op' fac_op ->
        let facs, rest = List.partition_tf ~f:(fun e -> equal e fac_expr) args' in
        (match facs with
        | [] -> Either.Second arg
        | _ :: tl -> Either.First (mk_e_assoc op' (tl @ rest)))
      | _ ->
        if equal arg fac_expr
        then (
          match get_id_const fac_op with
          | Some e -> Either.First e
          | _ -> Either.Second arg)
        else Either.Second arg
    in
    List.partition_map ~f args
  in
  match args_with_fac with
  | [] -> normalize (mk_e_assoc op args_no_fac)
  | _ ->
    let factorized = mk_e_assoc fac_op [ fac_expr; mk_e_assoc op args_with_fac ] in
    normalize (mk_e_assoc op (factorized :: args_no_fac))
;;

let apply_ite_factor
    (cond : t)
    (true_br : t)
    (false_br : t)
    (fac_op : Operator.t)
    (fac_expr : t)
  =
  let f arg =
    match arg with
    | EOp (op', args') when Operator.equal op' fac_op ->
      let facs, rest = List.partition_tf ~f:(fun e -> equal e fac_expr) args' in
      (match facs with
      | [] -> None
      | _ :: tl ->
        (match tl @ rest with
        | [] -> None
        | _ as args -> Some (mk_e_assoc op' args)))
    | _ ->
      if equal arg fac_expr
      then (
        match get_id_const fac_op with
        | Some e -> Some e
        | _ -> None)
      else None
  in
  let true_br_defac = f true_br
  and false_br_defac = f false_br in
  match true_br_defac, false_br_defac with
  | Some tbr, Some fbr ->
    Some (normalize (mk_e_assoc fac_op [ fac_expr; mk_e_ite cond tbr fbr ]))
  | _ -> None
;;

(** Factorize a term using distributivity rules. *)
let factorize (e : t) : t =
  let fac_rule e =
    let case f e =
      match e with
      | EOp (op, args) ->
        let args' = List.map ~f args in
        let factors = collect_common_factors (Some op) args' in
        Option.bind (best_factor factors) ~f:(fun (fac_op_opt, fac_expr) ->
            apply_factor op args' fac_op_opt fac_expr)
      | EIte (cond, br_true, br_false) ->
        let cond = f cond
        and br_true = f br_true
        and br_false = f br_false in
        Option.(
          bind
            (best_factor (collect_common_factors None [ br_true; br_false ]))
            ~f:(fun (fac_op_opt, fac_expr) ->
              bind fac_op_opt ~f:(fun fac_op ->
                  apply_ite_factor cond br_true br_false fac_op fac_expr)))
      | _ -> None
    in
    normalize (transform case e)
  in
  rewrite_until_stable fac_rule (normalize e)
;;

let distrib (op1 : Operator.t) (args : t list) : t =
  let f hd tl =
    mk_e_assoc
      op1
      (List.fold ~init:[ hd ] tl ~f:(fun acc e' ->
           match e' with
           | EOp (op2, args) ->
             if is_left_distrib op1 op2
             then
               [ mk_e_assoc
                   op2
                   (List.map args ~f:(fun arg -> mk_e_assoc op1 (acc @ [ arg ])))
               ]
             else acc @ [ e' ]
           | EIte (cond, true_br, false_br) ->
             [ mk_e_ite
                 cond
                 (mk_e_assoc op1 (acc @ [ true_br ]))
                 (mk_e_assoc op1 (acc @ [ false_br ]))
             ]
           | _ -> acc @ [ e' ]))
  in
  let args = if is_commutative op1 then List.sort ~compare args else args in
  match args with
  | [ a ] -> mk_e_assoc op1 [ a ]
  | hd :: tl -> f hd tl
  | _ -> failwith "Empty application."
;;

(** Expand a term by applying distributivity rules.*)
let expand (e : t) : t =
  let expand_rule e =
    let case f e =
      match e with
      | EOp (op1, args) -> Some (distrib op1 (List.map ~f args))
      | _ -> None
    in
    normalize (transform case e)
  in
  rewrite_until_stable expand_rule e
;;

let eequals a b =
  equal (expand (factorize (normalize a))) (expand (factorize (normalize b)))
;;

(** Rewrite rule out of a lemma. *)
let rewrite_with_lemma (lemma : t) : t -> t list =
  let conjs =
    match expand lemma with
    | EOp (Binary And, args) -> args
    | lemma' -> [ lemma' ]
  in
  let dyn_rules =
    let r e =
      let open Operator in
      match e with
      (* a >= x : a -> max a x *)
      | EOp (Binary Ge, [ a; EInt x ]) | EOp (Binary Le, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Max) [ a; EInt x ] ]
      | EOp (Binary Gt, [ a; EInt x ]) | EOp (Binary Lt, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Max) [ a; EInt (x + 1) ] ]
      (* a <= x : a -> min a x *)
      | EOp (Binary Le, [ a; EInt x ]) | EOp (Binary Ge, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Min) [ a; EInt x ] ]
      | EOp (Binary Lt, [ a; EInt x ]) | EOp (Binary Gt, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Min) [ a; EInt (x - 1) ] ]
      | _ -> []
    in
    List.concat_map ~f:r conjs
  in
  let apply_dyn_rule e =
    List.map dyn_rules ~f:(fun (lhs, rhs) ->
        let e' = transform (fun _ e -> if eequals lhs e then Some rhs else None) e in
        e')
  in
  apply_dyn_rule
;;

let match_core (bid : boxkind) (sube : t) : t -> t =
  let transformer _ e0 =
    if eequals sube e0
    then Some (EBox bid)
    else (
      match e0, sube with
      | EOp (op, args), EOp (subop, subargs) when Operator.equal op subop ->
        let subargs', rest =
          List.partition_tf ~f:(List.mem ~equal:eequals subargs) args
        in
        if List.length subargs' = List.length subargs
        then Some (mk_e_assoc op (EBox bid :: rest))
        else None
      (* | EInt i1, EInt i2 ->
        Some (mk_e_assoc (Binary Binop.Plus) [ mk_e_int (i2 - i1); EBox bid ]) *)
      | _ -> None)
  in
  transform transformer
;;

let match_as_is (bid : boxkind) (sube : t) ~(of_ : t) : t option =
  let res = match_core bid sube of_ in
  if count_boxkind bid res > 0 then Some res else None
;;

(** Matching subexpressions (up to rewriting) *)
let match_after_expand ?(lemma = None) (bid : boxkind) (sube : t) ~(of_ : t) : t option =
  (* Expand expressions. *)
  let of_ = expand of_
  and sube = expand sube in
  (* Use lemmas to rewrite expressions.  *)
  let of_choices =
    let choices_from_lemma_r =
      Option.value
        (Option.map
           ~f:(fun l -> of_ :: List.map ~f:expand (rewrite_with_lemma l of_))
           lemma)
        ~default:[ of_ ]
    in
    List.remove_consecutive_duplicates ~equal:eequals choices_from_lemma_r
  in
  match List.filter_map ~f:(fun of_ -> match_as_is bid sube ~of_) of_choices with
  | hd :: _ -> Some hd
  | _ -> None
;;

let match_as_subexpr ?(lemma = None) (bid : boxkind) (sube : t) ~(of_ : t) : t option =
  match match_after_expand ~lemma bid sube ~of_ with
  | Some hd -> Some hd
  | None -> match_as_is bid (factorize sube) ~of_:(factorize of_)
;;

let simplify_term ~(ctx : Term.Context.t) (t : Term.term) : Term.term =
  let simpl =
    let ctx = RContext.create ctx in
    let%bind expr = of_term ~ctx t in
    to_term ~ctx (normalize expr)
  in
  match simpl with
  | Some s -> s
  | None -> t
;;
