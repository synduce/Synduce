open ProblemDefs
open Base
open Env
open Utils
open Lang
open Term
open Option.Let_syntax

let no_synth () =
  Log.info (fun fmt () -> Fmt.pf fmt "No synthesis objective found, nothing to do!");
  Caml.exit 0
;;

let sync_args ~(ctx : Context.t) p : PsiDef.t =
  let subs =
    match List.zip p.PsiDef.reference.pargs p.PsiDef.target.pargs with
    | Unequal_lengths ->
      failwith
        "Reference and target recursion scheme must have the same number of parameters."
    | Ok var_subs -> List.map ~f:(fun (v1, v2) -> mk_var ctx v2, mk_var ctx v1) var_subs
  in
  let target' =
    PMRS.subst_rule_rhs
      ~ctx
      ~p:{ p.PsiDef.target with pargs = p.PsiDef.reference.pargs }
      subs
  in
  { p with target = target' }
;;

let find_problem_components
    ~(ctx : env)
    ~(filename : string)
    ((target_fname, spec_fname, repr_fname) : string * string * string)
    (pmrs_map : (string, PMRS.t, String.comparator_witness) Map.t)
    : PsiDef.t
  =
  (* Representation function. *)
  let repr, theta_to_tau =
    match Map.find pmrs_map repr_fname with
    | Some pmrs -> Either.First pmrs, var_type ctx pmrs.pmain_symb
    | None ->
      let reprs =
        Hashtbl.filter
          ~f:(fun (v, _, _, _) -> String.(v.vname = repr_fname))
          ctx.ctx.globals
      in
      (match Hashtbl.choose reprs with
      | Some (_, (f, a, _, b)) -> Either.Second (f, a, b), var_type ctx f
      (* No repr specified: assume identity. *)
      | None ->
        let x = Variable.mk ctx.ctx "x" in
        let xt = var_type ctx x in
        let repr_fun = Variable.mk ctx.ctx ~t:(Some (TFun (xt, xt))) repr_fname in
        Either.Second (repr_fun, [ FPatVar x ], mk_var ctx.ctx x), RType.TFun (xt, xt))
  in
  (* Reference function. *)
  let reference_f, tau =
    match Map.find pmrs_map spec_fname with
    | Some pmrs ->
      (try pmrs, PMRS.extract_rec_input_typ pmrs with
      | _ ->
        Log.error_msg
          Fmt.(str "Reference function should have at least one input argument.");
        no_synth ())
    | None ->
      Log.error_msg Fmt.(str "No spec named %s found." spec_fname);
      no_synth ()
  in
  (* Target recursion scheme. *)
  let target_f, theta_0 =
    let target_f =
      match Map.find pmrs_map target_fname with
      | Some pmrs -> pmrs
      | None ->
        Log.error_msg Fmt.(str "No recursion skeleton named %s found." target_fname);
        no_synth ()
    in
    try target_f, PMRS.extract_rec_input_typ target_f with
    | _ ->
      Log.error_msg Fmt.(str "Recursion skeleton should have at least one input.");
      no_synth ()
  in
  (* Match origin and target recursion scheme types. *)
  let subtheta =
    match RType.fun_typ_unpack theta_to_tau with
    | [ theta' ], tau' ->
      ctx >- PMRS.unify_two_with_vartype_update (theta_0, theta') (tau, tau')
    | _ ->
      Log.error_msg "Representation function should be a function.";
      Log.fatal ()
  in
  let theta = RType.(sub_all (mkv subtheta) theta_0) in
  Term.(
    let reference_out = var_type ctx reference_f.pmain_symb in
    let target_out = var_type ctx target_f.pmain_symb in
    Log.debug_msg
      Fmt.(str "ɑ : unify %a and %a" RType.pp reference_out RType.pp target_out);
    match reference_out, target_out with
    | TFun (_, tout), TFun (_, tout') ->
      (match RType.unify_one tout tout' with
      | Ok subs -> Variable.update_var_types ctx.ctx (RType.mkv subs)
      | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        Log.error_msg "Failed to unify output types.";
        no_synth ())
    | _ ->
      Log.error_msg "Original or target is not a function.";
      no_synth ());
  (*  Update the type of all the components. *)
  let repr =
    match repr with
    | Either.First pmrs -> Either.First (ctx >- PMRS.infer_pmrs_types pmrs)
    | Either.Second (f, a, b) ->
      let b', _ = Term.infer_type ctx.ctx b in
      Either.Second (f, a, b')
  in
  let target_f = ctx >- PMRS.infer_pmrs_types target_f in
  let reference_f = ctx >- PMRS.infer_pmrs_types reference_f in
  let t_out = reference_f.poutput_typ in
  let repr_pmrs =
    match repr with
    | Either.First p -> p
    | Either.Second (f, a, b) -> ctx >- PMRS.func_to_pmrs f a b
  in
  let tinv_pmrs =
    let%bind spec = ctx >- Specifications.get_spec target_f.pvar in
    let%bind t = spec.requires in
    match t.tkind with
    | TVar func_var -> PMRS.Functions.find_global ctx.functions func_var.vid
    | _ -> None
  in
  let problem =
    ctx
    >- sync_args
         PsiDef.
           { filename
           ; id = ProblemDefs.PsiDef.new_psi_id ()
           ; target = target_f
           ; reference = reference_f
           ; repr = repr_pmrs
           ; tinv = tinv_pmrs
           ; repr_is_identity = ctx >>- Reduce.is_identity repr_pmrs
           ; lifting = []
           }
  in
  (* Print summary information about the problem, before solving.*)
  ctx >- AlgoLog.show_summary (spec_fname, repr_fname, target_fname) target_f;
  (* Print reference function. *)
  ctx >- AlgoLog.show_pmrs problem.PsiDef.reference;
  (* Print target recursion skeleton. *)
  ctx >- AlgoLog.show_pmrs problem.PsiDef.target;
  (* Print representation function. *)
  Log.info
    Fmt.(
      fun fmt () ->
        match repr with
        | Either.First pmrs -> ctx >- AlgoLog.show_pmrs pmrs
        | Either.Second (fv, args, body) ->
          pf
            fmt
            "%s(%a) = %a"
            fv.vname
            (list ~sep:comma (Term.pp_fpattern ctx.ctx))
            args
            (Term.pp_term ctx.ctx)
            body);
  Log.verbose (ctx >- Specifications.dump_all);
  (* Print the condition on the reference function's input, if there is one. *)
  (match problem.tinv with
  | Some tinv -> ctx >- AlgoLog.show_pmrs tinv
  | None -> ());
  (* Set global information. *)
  ctx.tau := tau;
  ctx.theta := theta;
  ctx.alpha := t_out;
  problem
;;

let update_context ~(ctx : env) (p : PsiDef.t) =
  let target = ctx >- PMRS.infer_pmrs_types p.PsiDef.target in
  PMRS.Functions.update ctx.functions target;
  ctx.refinement_steps := 0
;;
