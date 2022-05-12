open Base
open Common
open ProblemDefs
open Env
open Lang
open Term
open Syguslib.Sygus
open Utils

let next_lifing_type : RType.t list ref = ref [ RType.TInt ]

(* ============================================================================================= *)
(*                      LIFTING UTILS                                                            *)
(* ============================================================================================= *)

let msg_lifting () =
  Log.info
    Fmt.(
      fun frmt () ->
        (styled
           (`Fg `Black)
           (styled (`Bg (`Hi `Red)) (fun frmt () -> pf frmt "Lifting ... ")))
          frmt
          ())
;;

module LiftingMap = struct
  type t = ((int * term) * term) list

  let empty : t = []

  let key_match ~ctx ~key:(i1, t1) i2 t2 =
    if i1 = i2 then Matching.matches ~ctx ~pattern:t1 t2 else None
  ;;

  let get ~ctx (map : t) ((i, t0) : int * term) =
    List.find_map map ~f:(fun ((i1, t1), tr) ->
        match key_match ~ctx ~key:(i1, t1) i t0 with
        | Some sub -> Some (substitution (VarMap.to_subst ctx sub) tr)
        | None -> None)
  ;;

  let is_empty (a : t) = List.is_empty a
  let set (map : t) ((i, t0) : int * term) (tr : term) = ((i, t0), tr) :: map
end

let empty_lifting = { tmap = LiftingMap.empty }
let is_empty_lifting lifting = LiftingMap.is_empty lifting.tmap

let alpha_component_count env =
  match get_alpha env with
  | RType.TTup tl -> List.length tl
  | _ -> 1
;;

let decompose_t ~(ctx : env) (p : PsiDef.t) (t : term) : (int * term) option =
  let g = p.PsiDef.target.pmain_symb in
  match t.tkind with
  | TSel (t', i) ->
    let n = alpha_component_count ctx in
    if i >= n
    then (
      match t'.tkind with
      | TApp ({ tkind = TVar f; _ }, [ targ ]) when Variable.(equal f g) -> Some (i, targ)
      | _ -> None)
    else None
  | _ -> None
;;

let recompose_t ~(ctx : Context.t) (p : PsiDef.t) (t : term) (i : int) : term =
  let g = p.PsiDef.target.pmain_symb in
  mk_sel ctx (mk_app (mk_var ctx g) [ t ]) i
;;

(**
  Find the expression of the lifting for an input term.
*)
let get_mapped_value ~(ctx : env) ~(p : PsiDef.t) (l : lifting) (t : term) =
  Option.(decompose_t ~ctx p t >>= (ctx >- LiftingMap.get l.tmap))
;;

(**
  Interactively add an expression for the lifting.
*)
let interactive_add_lifting_expression
    ~(ctx : env)
    ~(p : PsiDef.t)
    (l : lifting)
    (a : term)
    (i : int)
    : lifting * term option
  =
  let env = VarSet.to_env (ctx >- Analysis.free_variables a) in
  let g = p.PsiDef.target.pmain_symb in
  (* Prompt the user to add lemmas. *)
  Log.info (fun frmt () ->
      Fmt.(pf frmt "Please provide a value for (%s %a).%i:" g.vname (pp_term ctx.ctx) a i));
  match Stdio.In_channel.input_line Stdio.stdin with
  | None | Some "" ->
    Log.info (fun frmt () -> Fmt.pf frmt "No lifting provided.");
    l, None
  | Some x ->
    let smtterm =
      try
        let sexpr = Sexplib.Sexp.of_string x in
        Smtlib.SmtLib.smtTerm_of_sexp sexpr
      with
      | Failure _ -> None
    in
    let l_term = Option.map ~f:(ctx >>- SmtInterface.term_of_smt env) smtterm in
    (match l_term with
    | None -> l, None
    | Some x ->
      (match ctx >- LiftingMap.get l.tmap (i, a) with
      | Some _ -> l, Some x
      | None -> { tmap = LiftingMap.set l.tmap (i, a) x }, Some x))
;;

let replace_boxed_expressions ~(ctx : env) ~(p : PsiDef.t) (l : lifting) =
  let case _ t =
    match t.tkind with
    | TBox t' -> get_mapped_value ~ctx ~p l t'
    | _ -> None
  in
  transform ~case
;;

(** A problem definition is lifted if the ouput type of the target recursion skeleton
  is different from the output type of the reference function (which is unchanged
  and stored in Common.ProblemDefs._alpha)
*)
let is_lifted ~(ctx : env) (p : PsiDef.t) : bool =
  let _, tout = RType.fun_typ_unpack (var_type ctx p.PsiDef.target.pvar) in
  not (Result.is_ok (RType.unify_one tout (get_alpha ctx)))
;;

let lifting_types ~(ctx : env) (p : PsiDef.t) : RType.t list =
  let _, tout = RType.fun_typ_unpack (var_type ctx p.PsiDef.target.pvar) in
  match tout with
  | RType.TTup tl ->
    let n = alpha_component_count ctx in
    if n > 0 then List.drop tl n else []
  | _ -> []
;;

let lifting_count ~(ctx : env) (p : PsiDef.t) : int = List.length (lifting_types ~ctx p)

(* ============================================================================================= *)
(*                       PROJECTIONS TO AND FROM LIFTING                                         *)
(* ============================================================================================= *)

(**
  Return a projection function that can be symbolically evaluated.
*)
let proj_to_non_lifting ~(ctx : env) (p : PsiDef.t) : term option =
  if not (is_lifted ~ctx p)
  then None
  else (
    let _, tout = RType.fun_typ_unpack (var_type ctx p.PsiDef.target.pvar) in
    match tout, get_alpha ctx with
    | TTup tl_lift, TTup tl' ->
      let args =
        List.map
          ~f:(fun t -> Variable.mk ctx.ctx (Alpha.fresh ctx.ctx.names ~s:"x") ~t:(Some t))
          tl_lift
      in
      let tuple_pattern = FPatTup (List.map ~f:(fun v -> FPatVar v) args) in
      let tup_orig =
        mk_tup ctx.ctx (List.map ~f:(mk_var ctx.ctx) (List.take args (List.length tl')))
      in
      Some (mk_fun ctx.ctx [ tuple_pattern ] tup_orig)
    | TTup tl_lift, _ ->
      let args =
        List.map
          ~f:(fun t -> Variable.mk ctx.ctx (Alpha.fresh ctx.ctx.names ~s:"x") ~t:(Some t))
          tl_lift
      in
      let tuple_pattern = FPatTup (List.map ~f:(fun v -> FPatVar v) args) in
      Some (mk_fun ctx.ctx [ tuple_pattern ] (mk_var ctx.ctx (List.hd_exn args)))
    | _ ->
      Log.error_msg "Ignoring type difference between ɑ and output of target.";
      None)
;;

let is_proj_function ~(ctx : env) (p : PsiDef.t) (t : term) : bool =
  match t.tkind with
  | TFun ([ tuple_arg_pattern ], tuple_body) ->
    (match tuple_arg_pattern with
    | FPatTup t ->
      if List.length t = lifting_count ~ctx p + alpha_component_count ctx
      then (
        match tuple_body.tkind with
        | TTup tl -> List.length tl = alpha_component_count ctx
        | _ -> alpha_component_count ctx = 1)
      else false
    | _ -> false)
  | _ -> false
;;

(**
    Return function f such that f(a) = a.i, .. a.i + n if i, .. i+n where
    i, ..., i+n are the components of the lifting.
*)
let proj_to_lifting ~(ctx : env) (p : PsiDef.t) : (term -> term) option =
  if not (is_lifted ~ctx p)
  then None
  else (
    let _, tout = RType.fun_typ_unpack (var_type ctx p.PsiDef.target.pvar) in
    match tout, get_alpha ctx with
    | TTup tl_lift, TTup tl' ->
      let n = List.length tl' in
      Some
        (fun a ->
          match
            List.mapi
              ~f:(fun i t -> mk_sel ctx.ctx ~typ:(Some t) a (n + i))
              (List.drop tl_lift n)
          with
          | [] -> mk_tup ctx.ctx []
          | [ x ] -> x
          | hd :: tl -> mk_tup ctx.ctx (hd :: tl))
    | TTup tl_lift, _ ->
      Some
        (fun a ->
          match
            List.mapi
              ~f:(fun i t -> mk_sel ctx.ctx ~typ:(Some t) a (1 + i))
              (List.drop tl_lift 1)
          with
          | [] -> mk_tup ctx.ctx []
          | [ x ] -> x
          | hd :: tl -> mk_tup ctx.ctx (hd :: tl))
    | _ ->
      Log.error_msg "Ignoring type difference between ɑ and output of target.";
      None)
;;

(**
  Returns a symbolic function [f] such that [f original_part lifting_part]
  computes the tuple that corresponds to the lifted function from the original components
  of the function in [oringal_part] and the components of the lifting only in [lifting_part]
*)
let compose_parts ~(ctx : env) (p : PsiDef.t) : term option =
  if not (is_lifted ~ctx p)
  then None
  else (
    let _, tout = RType.fun_typ_unpack (var_type ctx p.PsiDef.target.pvar) in
    match tout, get_alpha ctx with
    | TTup tl_lift, TTup tl' ->
      let args =
        List.map
          ~f:(fun t -> Variable.mk ctx.ctx (Alpha.fresh ~s:"x" ctx.ctx.names) ~t:(Some t))
          tl_lift
      in
      let n = List.length tl' in
      let tuple_pattern_orig =
        FPatTup (List.map ~f:(fun v -> FPatVar v) (List.take args n))
      in
      let tuple_pattern_lifting =
        match List.drop args n with
        | [ x ] -> FPatVar x
        | _ :: _ as l -> FPatTup (List.map ~f:(fun v -> FPatVar v) l)
        | _ -> failwith "Impossible."
      in
      let tup_all = mk_tup ctx.ctx (List.map ~f:(mk_var ctx.ctx) args) in
      Some (mk_fun ctx.ctx [ tuple_pattern_orig; tuple_pattern_lifting ] tup_all)
    | TTup tl_lift, _ ->
      let args =
        List.map
          ~f:(fun t -> Variable.mk ctx.ctx (Alpha.fresh ~s:"x" ctx.ctx.names) ~t:(Some t))
          tl_lift
      in
      let n = 1 in
      let pattern_orig = FPatVar (List.hd_exn args) in
      let tuple_pattern_lifting =
        match List.map ~f:(fun v -> FPatVar v) (List.drop args n) with
        | [ a ] -> a
        | _ as l -> FPatTup l
      in
      let tup_all = mk_tup ctx.ctx (List.map ~f:(mk_var ctx.ctx) args) in
      Some (mk_fun ctx.ctx [ pattern_orig; tuple_pattern_lifting ] tup_all)
    | _ ->
      Log.error_msg "Ignoring type difference between ɑ and output of target.";
      None)
;;

(** [nth_output_type p n] eturn the nth type in the output type of the PMRS.
    If the output type is a tuple, the nth element of the tuple is returned if it
      exists, otherwise [None] is returned.
    If the output type is not a tuple and [n] is not [0] then [None] is returned.
      If [n = 0] then the output type is returned.
 *)
let nth_output_type ~(ctx : Context.t) (p : PsiDef.t) (i : int) : RType.t option =
  let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new ctx p.PsiDef.target.pvar) in
  match tout with
  | RType.TTup tl -> List.nth tl i
  | _ -> if i = 0 then Some tout else None
;;

(* ============================================================================================= *)
(*                     HELPERS                                                                   *)
(* ============================================================================================= *)
let analyze_leftover ~(ctx : EProps.RContext.t) (expr : Expression.t) : unit =
  Log.verbose Fmt.(fun fmt () -> pf fmt "Analyzing leftover expression...@.");
  let expr' = Rewriter.factorize expr in
  Log.verbose Fmt.(fun fmt () -> pf fmt "-> Expr %a@." (Expression.pp ~ctx) expr');
  let need_to_compute = Deduction.subexpressions_without_boxes expr' in
  Log.verbose
    Fmt.(
      fun fmt () ->
        pf
          fmt
          "-> Lifting may also need to compute %a@."
          (list ~sep:comma (parens (Expression.pp ~ctx)))
          (Set.to_list need_to_compute))
;;

(* ============================================================================================= *)
(*                      LIFTING FUNCTIONS                                                        *)
(* ============================================================================================= *)

(**
  [apply_lifting ~ctx ~p l] lifts [p.PsiDef.target] by extending the output with [l].
*)
let apply_lifting ~(ctx : env) ~(p : PsiDef.t) (new_lifting : RType.t list) : PsiDef.t =
  (* Type inference on p.target to update types *)
  let target' =
    let new_out_type =
      let open RType in
      match snd (fun_typ_unpack (var_type ctx p.PsiDef.target.PMRS.pvar)) with
      | TTup old_ts -> TTup (old_ts @ new_lifting)
      | t_out -> TTup (t_out :: new_lifting)
    in
    let target' =
      ctx >- PMRS.infer_pmrs_types (ctx >- PMRS.clear_pmrs_types p.PsiDef.target)
    in
    let free_theta = PMRS.extract_rec_input_typ target' in
    (* Update the input type, w.r.t to the the theta stored. *)
    ctx >- PMRS.unify_one_with_update (free_theta, get_theta ctx);
    (* Update the output type. *)
    (match
       RType.unify_one
         (snd (RType.fun_typ_unpack (var_type ctx target'.pmain_symb)))
         new_out_type
     with
    | Ok subs -> Variable.update_var_types ctx.ctx (RType.mkv subs)
    | Error e ->
      Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
      Log.error_msg "Failed to unify output types in lifting.");
    ctx >- PMRS.infer_pmrs_types target'
  in
  (* ⚠️ !TODO! : updates all the "ensures" *)
  Log.debug (fun ft () ->
      Fmt.(pf ft "@[After lifting:@;%a@]" (box (ctx >- PMRS.pp ~short:true)) target'));
  PsiDef.{ p with target = target'; lifting = p.lifting @ new_lifting }
;;

let lift_interactive ~(ctx : env) ~p lifting boxes =
  List.fold
    ~init:lifting
    ~f:(fun l (i, t) ->
      match ctx >- LiftingMap.get l.tmap (i, t) with
      | Some _ -> l
      | None -> fst (interactive_add_lifting_expression ~ctx ~p l t i))
    boxes
;;

let deduce_lifting_expressions
    ~(ctx : env)
    ~(p : PsiDef.t)
    (lifting : lifting)
    (lemma : term option)
    ~(lhs : term)
    ~(rhs : term)
    : lifting
  =
  (* Put every recursive call to the lifting part into a "box" *)
  let boxes =
    reduce
      ~init:[]
      ~case:(fun _ t ->
        match decompose_t ~ctx p t with
        | Some (i, a) -> Some [ i, a ]
        | _ -> None)
      ~join:( @ )
      rhs
  in
  if !Config.interactive_lifting
  then lift_interactive ~p ~ctx lifting boxes
  else (
    let subs, boxvar_to_linput =
      (* Create fresh variables for boxes and a map from var id to box argument. *)
      boxes
      |> List.map ~f:(fun (i, box_input) ->
             match ctx >- LiftingMap.get lifting.tmap (i, box_input) with
             | Some e -> (ctx >- recompose_t p box_input i, e), None
             | None ->
               let x =
                 Variable.mk
                   ctx.ctx
                   (Alpha.fresh ctx.ctx.names ~s:(Fmt.str "_L_%i" i))
                   ~t:(ctx >- nth_output_type p i)
               in
               ( (ctx >- recompose_t p box_input i, mk_var ctx.ctx x)
               , Some (x, (i, box_input)) ))
      |> List.unzip
    in
    let boxvar_to_linput = List.filter_opt boxvar_to_linput in
    match
      rhs
      |> substitution subs
      |> Deduction.as_unknown_app
           ~match_functions:(is_proj_function ~ctx p)
           ~unknowns:p.PsiDef.target.psyntobjs
    with
    | Some rhs_args ->
      let var_to_lifting_expr, maybe_leftover_expr, sol_ctx =
        let f (x, (_, t)) =
          (* A box completion might also use the parameters of the function. *)
          ( x
          , Set.union
              (ctx >- Analysis.free_variables t)
              (VarSet.of_list p.PsiDef.target.pargs) )
        in
        boxvar_to_linput
        |> List.map ~f
        |> Deduction.Solver.functional_equation ~ctx ~func_side:rhs_args ~lemma lhs
      in
      let _ = Option.map ~f:(analyze_leftover ~ctx:sol_ctx) maybe_leftover_expr in
      List.fold var_to_lifting_expr ~init:lifting ~f:(fun l (v, t) ->
          let i, t0 = List.Assoc.find_exn ~equal:Variable.equal boxvar_to_linput v in
          { tmap = LiftingMap.set l.tmap (i, t0) t })
    | None -> lifting)
;;

(* ============================================================================================= *)
(*                       MAIN ENTRY POINT                                                        *)
(* ============================================================================================= *)

(**
  Perform a scalar lifting.
*)
let scalar
    ~(ctx : env)
    ~(p : PsiDef.t)
    (l : refinement_loop_state)
    ((_s_resp, synt_failure_info) :
      solver_response * ('a, unrealizability_witness list) Either.t)
    : (PsiDef.t * refinement_loop_state, solver_response) Result.t
  =
  (* Select the type of lifting from the synt_failure_info.  *)
  let lifting_type =
    let m uc =
      let hint_1 =
        let common_vars = Set.union uc.ci.witness_vars uc.cj.witness_vars in
        let f type_decision var =
          let val_in_i = Map.find uc.ci.witness_model var
          and val_in_j = Map.find uc.cj.witness_model var in
          match val_in_i, val_in_j with
          | Some vi, Some vj ->
            if not (Terms.equal vi vj) then Some (var_type ctx var) else type_decision
          | Some _, _ | _, Some _ ->
            Option.first_some type_decision (Some (var_type ctx var))
          | _ -> type_decision
        in
        Set.fold common_vars ~f ~init:None
      in
      Option.value ~default:RType.TInt hint_1
    in
    let join l type_candidate =
      if Caml.List.mem type_candidate l then l else type_candidate :: l
    in
    let f a uc = join a (m uc) in
    match synt_failure_info with
    | First _ -> lifting_types ~ctx p @ !next_lifing_type
    | Second witness_list -> lifting_types ~ctx p @ List.fold ~f ~init:[] witness_list
  in
  Log.info
    Fmt.(
      fun fmt () ->
        pf fmt "Lifting with components %a" (list ~sep:comma RType.pp) lifting_type);
  (* Change the type of the functions. The actual lifting expressions will be computed when solving
    for the equation systems.
   *)
  let p' = apply_lifting ~ctx ~p lifting_type in
  Ok (p', { l with assumptions = [] })
;;
