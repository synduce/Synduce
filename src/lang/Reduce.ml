open Core
open Term
open Utils

let _MAX = 1000

let until_irreducible (f : term -> term * bool) (t0 : term) : term =
  let steps = ref 0 in
  let rec apply_until_irreducible t =
    Int.incr steps;
    let t', reduced = f t in
    if reduced && !steps < _MAX then apply_until_irreducible t' else t'
  in
  apply_until_irreducible (fst (f t0))
;;

let steps_until_irreducible (f : term -> term * bool) (t0 : term) : term list =
  let steps = ref 0 in
  let rec apply_until_irreducible t =
    Int.incr steps;
    let t', reduced = f t in
    if reduced && !steps < _MAX then t :: apply_until_irreducible t' else [ t' ]
  in
  apply_until_irreducible t0
;;

module CondTree = struct
  type 'a t =
    | CBr of 'a
    | CIf of term * 'a t * 'a t

  let mk_if c a b = CIf (c, a, b)
  let mk_br t = CBr t

  let rec to_term (ct : term t) : term =
    match ct with
    | CBr t -> t
    | CIf (c, a, b) -> mk_ite c (to_term a) (to_term b)
  ;;

  let of_term (trm : term) : term t =
    let rec aux trm =
      match trm.tkind with
      | TIte (c, a, b) -> mk_if c (aux a) (aux b)
      | _ -> mk_br trm
    in
    aux trm
  ;;

  let map (ct : 'a t) ~(f : 'a -> 'b) : 'b t =
    let rec aux ct =
      match ct with
      | CIf (c, a, b) -> CIf (c, aux a, aux b)
      | CBr x -> CBr (f x)
    in
    aux ct
  ;;

  let all_or_none (ct : 'a option t) : 'a t option =
    let rec aux ct =
      match ct with
      | CBr oa -> Option.map ~f:mk_br oa
      | CIf (c, a, b) ->
        let a' = aux a
        and b' = aux b in
        Option.map ~f:(fun (a, b) -> mk_if c a b) (Option.both a' b')
    in
    aux ct
  ;;
end

(* ============================================================================================= *)
(*                                  TERM REDUCTION                                               *)
(* ============================================================================================= *)
let project_irreducible_terms (ctx : Context.t) (t : term) =
  let case frec t =
    match t.tkind with
    | TVar v ->
      (match Variable.vtype_or_new ctx v with
      | RType.TTup tl ->
        Some
          (mk_tup
             ctx
             (List.mapi tl ~f:(fun i typ -> mk_sel ctx ~typ:(Some typ) (mk_var ctx v) i)))
      | _ -> None)
    | TApp (f, args) ->
      let f' = frec f
      and args' = List.map ~f:frec args in
      let _, tout = RType.fun_typ_unpack (type_of f') in
      (match tout with
      | RType.TTup tl ->
        Some
          (mk_tup
             ctx
             (List.mapi tl ~f:(fun i typ ->
                  mk_sel ctx ~typ:(Some typ) (mk_app f' args') i)))
      | _ -> None)
    | _ -> None
  in
  transform ~case t
;;

type func_resolution =
  | FRFun of fpattern list * term
  | FRPmrs of PMRS.t
  | FRNonT of PMRS.t
  | FRUnknown

let resolve_func (fctx : PMRS.Functions.ctx) (ctx : Context.t) (func : term)
    : func_resolution
  =
  match func.tkind with
  | TVar x ->
    (match Hashtbl.find ctx.globals x.vname with
    | Some (_, vargs, _, body) -> FRFun (vargs, body)
    | None ->
      (match PMRS.Functions.find_global fctx x.vid with
      | Some pm -> FRPmrs pm
      | None ->
        (match PMRS.Functions.find_nonterminal fctx x.vid with
        | Some pm -> FRNonT pm
        | None -> FRUnknown)))
  | TFun (vargs, body) -> FRFun (vargs, body)
  | _ -> FRUnknown
;;

(** Looks for a set of applicable rules in prules to rewrite (f fargs) and return
    the result of applying the possible rules.
    If there is no rule that is applicable, then return an empty list.
*)
let rule_lookup (ctx : Context.t) prules (f : variable) (fargs : term list) : term list =
  let app_sub bindv bindto expr =
    let bindt = List.map ~f:(mk_var ctx) bindv in
    match List.map2 ~f:Utils.pair bindt bindto with
    | Ok x -> Some (substitution x expr)
    | _ -> None
  in
  let match_with_pat (_, rule_args, _, rhs) pat first_args to_pat_match =
    match Matching.matches_pattern to_pat_match pat with
    | Some bindto_map ->
      let bindto_list = Map.to_alist bindto_map in
      let pat_v, pat_bto = List.unzip bindto_list in
      app_sub (rule_args @ pat_v) (first_args @ pat_bto) rhs
    | None -> None
  in
  let f (nt, rule_args, rule_pat, rhs) =
    if Variable.(nt = f)
    then (
      match rule_pat with
      (* We have a pattern, try to match it. *)
      | Some pat ->
        (* Separate last argument and the rest. Last argument is pattern-matched. *)
        (match List.last fargs, List.drop_last fargs with
        | Some to_pat_match, Some first_args ->
          let f = match_with_pat (nt, rule_args, rule_pat, rhs) pat first_args in
          let cond_pat = CondTree.of_term to_pat_match in
          Option.map ~f:CondTree.to_term CondTree.(all_or_none (map ~f cond_pat))
        | _ -> None)
      (* Pattern is empty. Simple substitution. *)
      | None -> app_sub rule_args fargs rhs)
    else None
  in
  second (List.unzip (Map.to_alist (Map.filter_map prules ~f)))
;;

(**
  reduce_term reduces a term using only the lambda-calculus
*)
let rec reduce_term
    ?(projecting = false)
    ?(unboxing = false)
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (t : term)
    : term
  =
  let one_step t =
    let rstep = ref false in
    let case f t =
      let x =
        match t.tkind with
        | TApp (func, args) ->
          let func' = f func in
          let args' = List.map ~f args in
          (match resolve_func fctx ctx func' with
          | FRFun (fpatterns, body) ->
            (match Analysis.subst_args ~ctx fpatterns args' with
            | Ok (remaining_patterns, subst) ->
              (* If there are remaining patterns the application is partial. *)
              (match remaining_patterns with
              | [] -> Some (substitution subst body)
              | rem -> Some (mk_fun ctx rem (substitution subst body)))
            | Error _ -> None)
          | FRPmrs pm ->
            (match args' with
            | [ tp ] -> Some (f (reduce_pmrs ~fctx ~ctx pm tp))
            | _ -> None (* PMRS are defined only with one argument for now. *))
          | FRNonT p -> Some (pmrs_until_irreducible ~ctx p (mk_app func' args'))
          | FRUnknown -> Some (mk_app func' args'))
        | TFun ([], body) -> Some (f body)
        | TIte (c, tt, tf) ->
          (match c.tkind with
          (* Resolve constants *)
          | TConst Constant.CFalse -> Some (f tf)
          | TConst Constant.CTrue -> Some (f tt)
          (* Distribute ite on tuples *)
          | _ ->
            (match tt.tkind, tf.tkind with
            | TTup tlt, TTup tlf ->
              (match List.zip tlt tlf with
              | Ok zip ->
                Some (mk_tup ctx (List.map zip ~f:(fun (tt', tf') -> mk_ite c tt' tf')))
              | Unequal_lengths -> None)
            | _, _ -> None))
        | TSel (t, i) ->
          (match f t with
          | { tkind = TTup tl; _ } -> Some (List.nth_exn tl i)
          | _ -> None)
        | TMatch (t, cases) ->
          (match
             List.filter_opt
               (List.map
                  ~f:(fun (p, t') ->
                    Option.map ~f:(fun m -> m, t') (Matching.matches_pattern t p))
                  cases)
           with
          | [] -> None
          | (subst_map, rhs_t) :: _ ->
            Some (substitution (VarMap.to_subst ctx subst_map) rhs_t))
        | TBox t -> if unboxing then Some t else None
        | TFun _ | TVar _ | TTup _ | TBin _ | TUn _ | TConst _ | TData _ -> None
      in
      match x with
      | Some x ->
        rstep := true;
        Some x
      | None -> None
    in
    transform ~case (if projecting then project_irreducible_terms ctx t else t), !rstep
  in
  until_irreducible one_step t

and pmrs_until_irreducible ~(ctx : Context.t) (prog : PMRS.t) (input : term) =
  let one_step t0 =
    let rstep = ref false in
    let rewrite_rule tm =
      match tm.tkind with
      | TApp ({ tkind = TVar f; _ }, fargs) ->
        (match rule_lookup ctx prog.prules f fargs with
        | [] -> tm (* No rule matches *)
        | hd :: _ ->
          (* Only select first match. *)
          rstep := true;
          hd)
      | _ -> tm
    in
    let t0' = rewrite_with rewrite_rule t0 in
    t0', !rstep
  in
  until_irreducible one_step input

and reduce_pmrs
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (prog : PMRS.t)
    (input : term)
  =
  let f_input = mk_app (mk_var ctx prog.pmain_symb) [ input ] in
  reduce_term ~fctx ~ctx (pmrs_until_irreducible ~ctx prog f_input)
;;

(* ============================================================================================= *)
(*                                  DERIVED FROM REDUCTION                                       *)
(* ============================================================================================= *)

(**
  calc_term_step reduces a term using only the lambda-calculus
*)
let rec calc_term_step
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (rstep : bool ref)
    (t : term)
    : term
  =
  let case f t =
    match t.tkind with
    | TApp (func, args) ->
      let func' = f func
      and args' = List.map ~f args in
      (match resolve_func fctx ctx func' with
      | FRFun (fpatterns, body) ->
        (match Analysis.subst_args ~ctx fpatterns args' with
        | Ok (remaining_patterns, subst) ->
          (* If there are remaining patterns the application is partial. *)
          (match remaining_patterns with
          | [] -> Some (substitution subst body)
          | rem -> Some (mk_fun ctx rem (substitution subst body)))
        | Error _ -> None)
      | FRPmrs pm ->
        (match args' with
        | [ tp ] ->
          Some (f (pmrs_calc_one_step ctx rstep pm (mk_app (mk_var ctx pm.pvar) [ tp ])))
        | _ -> None (* PMRS are defined only with one argument for now. *))
      | FRNonT p -> Some (pmrs_calc_one_step ctx rstep p (mk_app func' args'))
      | FRUnknown -> None)
    | TFun ([], body) -> Some (f body)
    | TIte (c, tt, tf) ->
      (match c.tkind with
      (* Resolve constants *)
      | TConst Constant.CFalse -> Some (f tf)
      | TConst Constant.CTrue -> Some (f tt)
      (* Distribute ite on tuples *)
      | _ ->
        (match tt.tkind, tf.tkind with
        | TTup tlt, TTup tlf ->
          (match List.zip tlt tlf with
          | Ok zip ->
            Some (mk_tup ctx (List.map zip ~f:(fun (tt', tf') -> mk_ite c tt' tf')))
          | Unequal_lengths -> None)
        | _, _ -> None))
    | TSel (t, i) ->
      (match f t with
      | { tkind = TTup tl; _ } -> Some (List.nth_exn tl i)
      | _ -> None)
    | TMatch (t, cases) ->
      (match
         List.filter_opt
           (List.map
              ~f:(fun (p, t') ->
                Option.map ~f:(fun m -> m, t') (Matching.matches_pattern t p))
              cases)
       with
      | [] -> None
      | (subst_map, rhs_t) :: _ ->
        Some (substitution (VarMap.to_subst ctx subst_map) rhs_t))
    | TBox _ | TVar _ | TFun _ | TTup _ | TBin _ | TUn _ | TConst _ | TData _ -> None
  in
  transform ~case t

and pmrs_calc_one_step (ctx : Context.t) (rstep : bool ref) (prog : PMRS.t) (input : term)
    : term
  =
  let rewrite_rule tm =
    match tm.tkind with
    | TApp ({ tkind = TVar f; _ }, fargs) ->
      (match rule_lookup ctx prog.prules f fargs with
      | [] -> tm (* No rule matches *)
      | hd :: _ ->
        (* Only select first match. *)
        rstep := true;
        hd)
    | _ -> tm
  in
  let t0' = rewrite_with rewrite_rule input in
  t0'
;;

let calc_term ~(fctx : PMRS.Functions.ctx) ~(ctx : Context.t) (t : term) =
  let one_step (t0 : term) : term * bool =
    let rstep = ref false in
    let t0' = calc_term_step ~fctx ~ctx rstep t0 in
    t0', !rstep
  in
  steps_until_irreducible one_step t
;;

(* ============================================================================================= *)
(*                                  DERIVED FROM REDUCTION                                       *)
(* ============================================================================================= *)

let reduce_rules ~(fctx : PMRS.Functions.ctx) ~(ctx : Context.t) (p : PMRS.t) =
  let reduced_rules =
    let f (nt, args, pat, body) = nt, args, pat, reduce_term ~fctx ~ctx body in
    Map.map ~f p.prules
  in
  { p with prules = reduced_rules }
;;

let instantiate_with_solution
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (p : PMRS.t)
    (soln : (string * variable list * term) list)
  =
  let xi_set = p.psyntobjs in
  let xi_substs =
    let f (name, args, body) =
      match VarSet.find_by_name xi_set name with
      | Some xi ->
        (match args with
        | [] -> [ Term.mk_var ctx xi, body ]
        | _ ->
          [ Term.mk_var ctx xi, mk_fun ctx (List.map ~f:(fun x -> FPatVar x) args) body ])
      | None -> []
    in
    List.concat (List.map ~f soln)
  in
  let target_inst = PMRS.subst_rule_rhs ~ctx xi_substs ~p in
  PMRS.subst_rule_rhs ~ctx xi_substs ~p:(reduce_rules ~fctx ~ctx target_inst)
;;

let is_identity ~(fctx : PMRS.Functions.ctx) ~(ctx : Context.t) (p : PMRS.t) =
  match p.pinput_typ with
  | [ it ] ->
    let input_symb = Variable.mk ~t:(Some it) ctx "e" in
    (match reduce_pmrs ~fctx ~ctx p (mk_var ctx input_symb) with
    | { tkind = TVar x; _ } -> Variable.(x = input_symb)
    | _ -> false)
  | _ -> false
;;
