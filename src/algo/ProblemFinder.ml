open AState
open Base
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
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    ((target_fname, spec_fname, repr_fname) : string * string * string)
    (pmrs_map : (string, PMRS.t, String.comparator_witness) Map.t)
    : PsiDef.t
  =
  (* Representation function. *)
  let repr, theta_to_tau =
    match Map.find pmrs_map repr_fname with
    | Some pmrs -> Either.First pmrs, Variable.vtype_or_new ctx pmrs.pmain_symb
    | None ->
      let reprs =
        Hashtbl.filter ~f:(fun (v, _, _, _) -> String.(v.vname = repr_fname)) ctx.globals
      in
      (match Hashtbl.choose reprs with
      | Some (_, (f, a, _, b)) -> Either.Second (f, a, b), Variable.vtype_or_new ctx f
      (* No repr specified: assume identity. *)
      | None ->
        let x = Variable.mk ctx "x" in
        let xt = Variable.vtype_or_new ctx x in
        let repr_fun = Variable.mk ctx ~t:(Some (TFun (xt, xt))) repr_fname in
        Either.Second (repr_fun, [ FPatVar x ], mk_var ctx x), RType.TFun (xt, xt))
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
      PMRS.unify_two_with_vartype_update ctx (theta_0, theta') (tau, tau')
    | _ ->
      Log.error_msg "Representation function should be a function.";
      Log.fatal ()
  in
  let theta = RType.(sub_all (mkv subtheta) theta_0) in
  Term.(
    let reference_out = Variable.vtype_or_new ctx reference_f.pmain_symb in
    let target_out = Variable.vtype_or_new ctx target_f.pmain_symb in
    Log.debug_msg
      Fmt.(str "ɑ : unify %a and %a" RType.pp reference_out RType.pp target_out);
    match reference_out, target_out with
    | TFun (_, tout), TFun (_, tout') ->
      (match RType.unify_one tout tout' with
      | Ok subs -> Variable.update_var_types ctx (RType.mkv subs)
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
    | Either.First pmrs -> Either.First (PMRS.infer_pmrs_types ~ctx pmrs)
    | Either.Second (f, a, b) ->
      let b', _ = Term.infer_type ctx b in
      Either.Second (f, a, b')
  in
  let target_f = PMRS.infer_pmrs_types ~ctx target_f in
  let reference_f = PMRS.infer_pmrs_types ~ctx reference_f in
  let t_out = reference_f.poutput_typ in
  let repr_pmrs =
    match repr with
    | Either.First p -> p
    | Either.Second (f, a, b) -> PMRS.func_to_pmrs ~ctx f a b
  in
  let tinv_pmrs =
    let%bind spec = Specifications.get_spec target_f.pvar in
    let%bind t = spec.requires in
    match t.tkind with
    | TVar func_var -> PMRS.Functions.find_global fctx func_var.vid
    | _ -> None
  in
  let problem =
    sync_args
      ~ctx
      PsiDef.
        { id = AState.PsiDef.new_psi_id ()
        ; target = target_f
        ; reference = reference_f
        ; repr = repr_pmrs
        ; tinv = tinv_pmrs
        ; repr_is_identity = Reduce.is_identity ~fctx ~ctx repr_pmrs
        ; lifting = []
        }
  in
  (* Print summary information about the problem, before solving.*)
  AlgoLog.show_summary ~ctx (spec_fname, repr_fname, target_fname) target_f;
  (* Print reference function. *)
  AlgoLog.show_pmrs ~ctx problem.PsiDef.reference;
  (* Print target recursion skeleton. *)
  AlgoLog.show_pmrs ~ctx problem.PsiDef.target;
  (* Print representation function. *)
  Log.info
    Fmt.(
      fun fmt () ->
        match repr with
        | Either.First pmrs -> AlgoLog.show_pmrs ~ctx pmrs
        | Either.Second (fv, args, body) ->
          pf
            fmt
            "%s(%a) = %a"
            fv.vname
            (list ~sep:comma (Term.pp_fpattern ctx))
            args
            (Term.pp_term ctx)
            body);
  Log.verbose (Specifications.dump_all ~ctx);
  (* Print the condition on the reference function's input, if there is one. *)
  (match problem.tinv with
  | Some tinv -> AlgoLog.show_pmrs ~ctx tinv
  | None -> ());
  (* Set global information. *)
  AState._tau := tau;
  AState._theta := theta;
  AState._alpha := t_out;
  AState._span := List.length (Analysis.terms_of_max_depth ~ctx 1 theta);
  AState.refinement_steps := 0;
  problem
;;

let update_context ~(ctx : Context.t) ~(fctx : PMRS.Functions.ctx) (p : PsiDef.t) =
  let target = PMRS.infer_pmrs_types ~ctx p.PsiDef.target in
  PMRS.Functions.update fctx target;
  AState.refinement_steps := 0
;;
