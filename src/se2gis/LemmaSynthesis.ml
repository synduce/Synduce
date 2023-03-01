open Base
open Common
open Witnesses
open Env
open Lang
open LemmaVerif
open Term
open ProblemDefs
open SmtInterface
open SygusInterface
open Utils
module S = Smtlib.SmtLib
module Sy = Syguslib.Sygus
module Sm = Syguslib.Semantic

(* ============================================================================================= *)
(*                                  Creating and updating term states                            *)
(* ============================================================================================= *)

let term_info_of_witness
    ~(id : int)
    ~(ctx : env)
    ~(is_pos_witness : bool)
    (witness : witness)
    : term_info * cond_lemma
  =
  let scalar_vars = Map.keys witness.witness_model in
  let input_args_t = List.map ~f:(Variable.vtype_or_new ctx.ctx) scalar_vars in
  let lemma_f =
    Variable.mk
      ctx.ctx
      ~t:(Some (RType.fun_typ_pack input_args_t TBool))
      (Alpha.fresh ~s:"lemma" ctx.ctx.names)
  in
  let ti_formals =
    let formals = Map.keys witness.witness_model in
    if List.exists formals ~f:(fun v ->
           RType.is_recursive ctx.ctx.types (Variable.vtype_or_new ctx.ctx v))
    then failwith "Formal args of lemmas must be scalars!"
    else formals
  in
  ( { ti_context = ctx
    ; ti_psi_id = id
    ; ti_flag = false
    ; ti_term = witness.witness_eqn.eterm
    ; ti_elim = witness.witness_eqn.eelim
    ; ti_func = lemma_f
    ; ti_formals
    }
  , { cl_flag = false
    ; cl_cond = witness.witness_eqn.esplitter
    ; cl_lemmas = []
    ; cl_negatives = (if is_pos_witness then [] else [ witness.witness_model ])
    ; cl_positives = (if is_pos_witness then [ witness.witness_model ] else [])
    } )
;;

(**
  Returns a map from the variables in the witness to the variables in the term_info.
*)
let remap_witness ~(ctx : Context.t) (det : term_info) (wit : witness)
    : variable VarMap.t option
  =
  match Matching.alpha_equal ~ctx ~pattern:wit.witness_eqn.eterm det.ti_term with
  | Some wit_to_det ->
    let wts_subst = VarMap.to_subst ctx (Map.map ~f:(fun a -> mk_var ctx a) wit_to_det) in
    let remapped_wit_elim =
      List.map ~f:(fun (r, s) -> substitution wts_subst r, s) wit.witness_eqn.eelim
    in
    let wit_to_det =
      List.fold
        (Elim.subs_from_elim_to_elim ~ctx det.ti_elim remapped_wit_elim)
        ~init:wit_to_det
        ~f:(fun m (a, b) ->
          match a.tkind, b.tkind with
          | TVar a, TVar b -> Map.set m ~key:a ~data:b
          | _ -> m)
    in
    Some wit_to_det
  | None -> None
;;

let remap_model (wit : witness) (m : variable VarMap.t) =
  let wit_var_to_det_var a =
    match Map.find m a with
    | Some x -> x
    | None -> a
  in
  let f ~key ~data new_map = Map.set new_map ~key:(wit_var_to_det_var key) ~data in
  Map.fold wit.witness_model ~init:VarMap.empty ~f
;;

let remap_witness_model ~(ctx : Context.t) (det : term_info) (wit : witness) =
  Option.map ~f:(remap_model wit) (remap_witness ~ctx det wit)
;;

let cond_lemma_of_witness ~(ctx : Context.t) ~(pos : bool) (ti : term_info) (wi : witness)
  =
  match remap_witness ~ctx ti wi with
  | Some wmap ->
    let wm = remap_model wi wmap in
    let cl_cond =
      Option.map ~f:(substitution (VarMap.to_subst2 ctx wmap)) wi.witness_eqn.esplitter
    in
    Some
      { cl_flag = false
      ; cl_cond
      ; cl_lemmas = []
      ; cl_negatives = (if pos then [] else [ wm ])
      ; cl_positives = (if pos then [ wm ] else [])
      }
  | None -> None
;;

(* ============================================================================================= *)
(*                                  Lemma synthesis functions                                    *)
(* ============================================================================================= *)

(** Applying tinv to the term of a term info can help in findiing a good lemma.
  This function extract a skeleton for the lemma when the input is the application of
  tinv to the term of the term detail.
 *)
let skeleton_of_tinv
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (det : term_info)
    (tinv_of_t : term)
  =
  let arg_num var =
    Option.(List.findi ~f:(fun _ a -> Variable.equal a var) det.ti_formals >>| fst)
  in
  let _tuple_of_args args =
    VarSet.union_list
      (List.map
         ~f:(fun arg ->
           Analysis.free_variables
             ~ctx
             ~include_functions:false
             (substitution det.ti_elim arg))
         args)
  in
  let tinv_of_t' = Reduce.reduce_term ~ctx ~fctx ~projecting:true tinv_of_t in
  let case _ t =
    match t.tkind with
    | TSel ({ tkind = TApp (f, _); _ }, i) ->
      let _, tout = RType.fun_typ_unpack (type_of f) in
      (match tout with
      | RType.TTup tl ->
        (match List.nth tl i with
        | Some RType.TBool -> Some (mk_const (Constant.of_bool true))
        | Some RType.TInt -> Some (mk_const (Constant.of_int 0))
        | _ -> None)
      | _ -> None)
    | TApp (f, _) ->
      let _, tout = RType.fun_typ_unpack (type_of f) in
      (match tout with
      | RType.TBool -> Some (mk_const (Constant.of_bool true))
      | RType.TInt -> Some (mk_const (Constant.of_int 0))
      | _ -> None)
    | _ -> None
  in
  let tinv_of_t'' =
    transform_at_depth
      2
      ~case:(fun _ t ->
        match type_of t with
        | RType.TInt -> Some (mk_const (Constant.of_int 0))
        | RType.TBool -> Some (mk_const (Constant.of_bool true))
        | _ -> None)
      (transform ~case tinv_of_t')
  in
  (* ep_term tinv_of_t''); *)
  let rc = EProps.RContext.create ctx in
  let expr = Expression.of_term ~ctx:rc tinv_of_t'' in
  let expr_transform _ e =
    match e with
    | Expression.EVar v ->
      Option.(
        EProps.RContext.get_var rc v
        >>= arg_num
        >>| fun i -> Expression.EBox (Expression.Position i))
    | _ -> None
  in
  Option.bind
    ~f:(fun expr ->
      Skeleton.of_expression ~ctx:rc (Expression.transform expr_transform expr))
    expr
;;

(** Construct the synthesis objective corresponding to the lemma of the term state detail.
  For each synthesis strategy implemented, return a command that defines the synthesis
  objective (SyGuS command).
  All strategies should be executed in order to find a solution efficiently.
 *)
let synthfun_of_ti ~(p : PsiDef.t) (ti : term_info) : (Sy.command * string) list =
  let ctx = ti.ti_context.ctx in
  let fctx = ti.ti_context.functions in
  let opset =
    List.fold
      ~init:OpSet.empty
      ~f:(fun acc func -> Set.union acc (Analysis.operators_of func.f_body))
      (PMRS.func_of_pmrs ~ctx p.PsiDef.reference
      @ PMRS.func_of_pmrs ~ctx p.PsiDef.repr
      @
      match p.tinv with
      | None -> []
      | Some pmrs -> PMRS.func_of_pmrs ~ctx pmrs)
  in
  let gen_of_guess t guess =
    let lemma_requires_set_theory =
      List.exists ti.ti_formals ~f:(fun v ->
          requires_set_theory ~ctx (Variable.vtype_or_new ctx v))
    in
    let grammar =
      if lemma_requires_set_theory
      then None
      else
        Grammars.generate_grammar
          ~ctx
          ~short_predicate:t
          ~guess
          ~bools:true
          opset
          ti.ti_formals
          RType.TBool
    in
    let logic = if lemma_requires_set_theory then "ALL" else logic_of_operators opset in
    if List.exists ti.ti_formals ~f:(fun v ->
           RType.is_recursive ctx.types (Variable.vtype_or_new ctx v))
    then failwith "Formal args of lemmas must be scalars!"
    else mk_synthinv ~ctx ti.ti_func.vname ti.ti_formals grammar, logic
  in
  if !Config.Optims.make_partial_lemma_sketches
  then (
    let skeleton_guess =
      match p.tinv with
      | Some tinv ->
        skeleton_of_tinv ~fctx ~ctx ti (Reduce.reduce_pmrs ~ctx ~fctx tinv ti.ti_term)
      | _ -> None
    in
    [ gen_of_guess true skeleton_guess; gen_of_guess false skeleton_guess ])
  else [ gen_of_guess false None ]
;;

let term_var_string ~ctx term : string =
  match Set.elements (Analysis.free_variables ~ctx term) with
  | [] ->
    failwith
      (Fmt.str "Failed to extract string of variable name in term %a" (pp_term ctx) term)
  | var :: _ -> var.vname
;;

let convert_term_rec_to_witness_rec
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (det : term_info)
    (witness : witness)
    (name : string)
    : string
  =
  let rec g recvar lst =
    match lst with
    | [] ->
      failwith
        (Fmt.str "Could not find name %s in the witness's recursion elimination." name)
    | (a, b) :: tl ->
      let i = term_var_string ~ctx b in
      let l = term_var_string ~ctx a in
      if String.(equal l recvar) then i else g recvar tl
  in
  let rec h recvar n elim =
    match elim with
    | [] ->
      failwith
        (Fmt.str "Could not find witness rec elim entry for tuple entry named %s." name)
    | (a, b) :: tl ->
      let l = term_var_string ~ctx a in
      if String.(equal l recvar)
      then (
        match b.tkind with
        | TTup vars -> term_var_string ~ctx (List.nth_exn vars n)
        | _ ->
          failwith
            Fmt.(
              str
                "Cannot get tuple entry %s in witness rec elim; %s is not a tuple.)"
                name
                l))
      else h recvar n tl
  in
  let rec f lst =
    match lst with
    | [] ->
      failwith
        (Fmt.str "Could not find name %s in this term's recursion elimination." name)
    | (a, b) :: tl ->
      (match b.tkind with
      | TTup vars ->
        (match
           List.find_mapi vars ~f:(fun n x ->
               let l = term_var_string ~ctx a in
               let i = term_var_string ~ctx x in
               if String.(equal i name)
               then Some (h l n witness.witness_eqn.eelim)
               else None)
         with
        | Some s -> s
        | None -> f tl)
      | _ ->
        let i = term_var_string ~ctx b in
        let l = term_var_string ~ctx a in
        if String.(equal i name) then g l witness.witness_eqn.eelim else f tl)
  in
  match
    VarSet.find_by_name
      (Set.union
         (Analysis.free_variables ~ctx det.ti_term)
         (Set.union (VarSet.of_list p.PsiDef.reference.pargs) witness.witness_vars))
      name
  with
  | None -> f det.ti_elim
  | Some _ -> name
;;

let witness_model_to_args
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (det : term_info)
    (params : (string * Sy.sygus_sort) list)
    witness
    : Sy.sygus_term list
  =
  List.map params ~f:(fun (param_name, _) ->
      match
        let name = convert_term_rec_to_witness_rec ~ctx ~p det witness param_name in
        Map.find
          witness.witness_model
          (match
             VarSet.find_by_name
               (Set.union
                  (* Don't include functions, we won't get a model for them in CVC5. *)
                  (Analysis.free_variables ~ctx ~include_functions:false det.ti_term)
                  (Set.union
                     (VarSet.of_list p.PsiDef.reference.pargs)
                     witness.witness_vars))
               name
           with
          | None ->
            failwith
              (Fmt.str
                 "Failed to extract argument list from witness model (%s unknown)."
                 name)
          | Some v -> v)
      with
      | None ->
        Log.error Fmt.(fun fmt () -> pf fmt "I was looking for %s" param_name);
        Log.error
          Fmt.(fun fmt () -> pf fmt "The witness: %a" (Pretty.pp_witness ~ctx) witness);
        failwith "Failed to extract argument list from witness model."
      | Some t -> sygus_of_term ~ctx t)
;;

let constraint_of_neg_witness ~ctx (det : term_info) witness =
  let neg_constraint = mk_un Not (mk_app (mk_var ctx det.ti_func) (Map.data witness)) in
  Sy.mk_c_constraint (sygus_of_term ~ctx neg_constraint)
;;

let constraint_of_pos_witness ~ctx (det : term_info) witness =
  let pos_constraint = mk_app (mk_var ctx det.ti_func) (Map.data witness) in
  Sy.mk_c_constraint (sygus_of_term ~ctx pos_constraint)
;;

let log_soln ~ctx s vs t =
  Log.verbose (fun frmt () ->
      Fmt.pf
        frmt
        "Lemma candidate: \"%s %s = @[%a@]\"."
        s
        (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vs))
        (pp_term ctx)
        t)
;;

let handle_lemma_synth_response
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (ti : term_info)
    ((task, resolver) : Sy.solver_response option Lwt.t * int Lwt.u)
    : term list option Lwt.t
  =
  let parse_synth_fun (_, _, _, fbody) =
    let body, _ =
      infer_type
        ctx
        (term_of_sygus ~fctx ~ctx (VarSet.to_env (VarSet.of_list ti.ti_formals)) fbody)
    in
    body
  in
  match%lwt
    Lwt.wakeup resolver 0;
    task
  with
  | Some (RSuccess resps) ->
    let soln = List.map ~f:parse_synth_fun resps in
    let _ = List.iter ~f:(fun t -> log_soln ~ctx ti.ti_func.vname ti.ti_formals t) soln in
    Lwt.return (Some soln)
  | Some RInfeasible | Some RFail | Some RUnknown | None -> Lwt.return None
;;

let parse_positive_example_solver_model
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (det : term_info)
    (response : S.solver_response)
    : term VarMap.t list
  =
  match response with
  | S.SExps s ->
    let model = model_to_constmap ~ctx ~fctx (SExps s) in
    let m, _ =
      Map.partitioni_tf
        ~f:(fun ~key ~data:_ ->
          Option.is_some (VarSet.find_by_name (VarSet.of_list det.ti_formals) key))
        model
    in
    (* Remap the names to ids of the original variables in m' *)
    [ Map.fold
        ~init:VarMap.empty
        ~f:(fun ~key ~data acc ->
          match VarSet.find_by_name (VarSet.of_list det.ti_formals) key with
          | None ->
            Log.info (fun f () -> Fmt.(pf f "Could not find by name %s" key));
            acc
          | Some var -> Map.set ~data acc ~key:var)
        m
    ]
  | _ ->
    failwith
      "Parse model failure: Positive example cannot be found during lemma refinement."
;;

let synthesize_new_lemma ~(p : PsiDef.t) (ti : term_info) (cl : cond_lemma)
    : term option Lwt.t
  =
  let with_synth_obj i synth_obj logic =
    ti.ti_context >- AlgoLog.announce_new_lemma_synthesis i ti cl;
    let neg_constraints =
      List.map ~f:(ti.ti_context >- constraint_of_neg_witness ti) cl.cl_negatives
    in
    let pos_constraints =
      List.map ~f:(ti.ti_context >- constraint_of_pos_witness ti) cl.cl_positives
    in
    let extra_defs = Sm.[ max_definition; min_definition ] in
    let commands =
      Sy.mk_c_set_logic logic
      :: (extra_defs
         @ [ synth_obj ]
         @ neg_constraints
         @ pos_constraints
         @ [ Sy.mk_c_check_synth () ])
    in
    match%lwt
      ti.ti_context
      >>- handle_lemma_synth_response
            ti
            (SygusInterface.SygusSolver.solve_commands
               ~timeout:(Some !Config.Optims.wait_parallel_tlimit)
               commands)
    with
    | None -> Lwt.return None
    | Some solns -> Lwt.return (List.nth solns 0)
  in
  match synthfun_of_ti ~p ti with
  | [ (synth_obj, logic) ] -> with_synth_obj 0 synth_obj logic
  | obj_choices ->
    let lwt_tasks =
      List.mapi obj_choices ~f:(fun i (synth_obj, logic) ->
          Lwt.task ()
          |> fun (t, r) -> Lwt.bind t (fun _ -> with_synth_obj i synth_obj logic), r)
    in
    Lwt.pick
      (List.map
         ~f:(fun (t, r) ->
           Lwt.wakeup r 0;
           t)
         lwt_tasks)
;;

(* ============================================================================================= *)
(*                                  Main entry points                                            *)
(* ============================================================================================= *)

let rec lemma_refinement_loop
    ~(ctx : env)
    ~(p : PsiDef.t)
    (ti : term_info)
    (cl : cond_lemma)
    : cond_lemma option Lwt.t
  =
  match%lwt synthesize_new_lemma ~p ti cl with
  | None ->
    Log.debug_msg "Lemma synthesis failure.";
    Lwt.return None
  | Some lemma_term ->
    if !Config.interactive_check_lemma
    then
      ctx
      >- LemmasInteractive.interactive_check_lemma
           (lemma_refinement_loop ~ctx ~p ti)
           ti.ti_func.vname
           ti.ti_formals
           ti
           cl
           lemma_term
    else (
      match%lwt verify_lemma_candidate ~p ti cl lemma_term with
      (* The candidate lemma has been proved correct. *)
      | vmethod, Unsat ->
        let lemma =
          match cl.cl_cond with
          | None -> lemma_term
          | Some pre -> Terms.(pre => lemma_term)
        in
        ctx >- AlgoLog.lemma_proved_correct vmethod ti lemma;
        Lwt.return (Some { cl with cl_flag = true; cl_lemmas = lemma :: cl.cl_lemmas })
      (* The candidate lemmas has not been proved correct. *)
      | vmethod, S.SExps x ->
        AlgoLog.lemma_not_proved_correct vmethod;
        let new_positive_witnesss =
          ctx >>- parse_positive_example_solver_model ti (S.SExps x)
        in
        List.iter
          ~f:(fun witness ->
            Log.verbose (fun f () ->
                Fmt.(
                  pf
                    f
                    "Found a positive example: %a"
                    (box (ctx @>- pp_subs))
                    (VarMap.to_subst ctx.ctx witness))))
          new_positive_witnesss;
        lemma_refinement_loop
          ~ctx
          ~p
          ti
          { cl with cl_positives = cl.cl_positives @ new_positive_witnesss }
      | _, Sat ->
        Log.error_msg "Lemma verification returned Sat. This is unexpected.";
        Lwt.return None
      | _, Unknown ->
        Log.error_msg "Lemma verification returned Unknown.";
        Lwt.return None
      | _ ->
        Log.error_msg "Lemma verification is indeterminate.";
        Lwt.return None)
;;

(** Partitioning function to partitiion a list into (a,b,c) where a are
  examples that satisfy the invariant,
  b are examples that do not satisfy the invariant,
  c are examples that are spurious for other reasons.
*)
let witnesss_for_lemma_synt witness =
  match witness.witness_stat with
  | Valid -> `Fst witness
  | Spurious causes ->
    if Caml.List.mem ViolatesTargetRequires causes then `Snd witness else `Trd witness
  | _ -> `Trd witness
;;

(** Partitioning function to partitiion a list into (a,b,c) where a are
  examples that are not in the reference function's image,
  b are examples that are in the reference function's image,
  c are examples that are spurious for other reasons.
*)
let witnesss_for_ensures_synt witness =
  match witness.witness_stat with
  | Valid -> `Fst witness
  | Spurious causes ->
    if Caml.List.mem NotInReferenceImage causes then `Snd witness else `Trd witness
  | _ -> `Trd witness
;;

let refine_ensures_predicates
    ~(ctx : env)
    ~(p : PsiDef.t)
    ~(neg_witnesss : witness list)
    ~(pos_witnesss : witness list)
    : [ `CoarseningOk | `CoarseningFailure | `Unrealizable ] Lwt.t
  =
  Log.info
    Fmt.(
      fun fmt () ->
        pf fmt "%i counterexamples violate image assumption." (List.length neg_witnesss));
  let%lwt maybe_pred = ImagePredicates.synthesize ~ctx ~p pos_witnesss neg_witnesss [] in
  match maybe_pred with
  | None -> Lwt.return `CoarseningFailure
  | Some ensures ->
    (match ctx >- Specifications.get_ensures p.PsiDef.reference.pvar with
    | None ->
      AlgoLog.show_new_ensures_predicate ~ctx p.PsiDef.reference.pvar ensures;
      ctx >- Specifications.set_ensures p.PsiDef.reference.pvar ensures
    | Some old_ensures ->
      let var : variable =
        Variable.mk
          ctx.ctx
          ~t:(Some p.PsiDef.reference.poutput_typ)
          (Alpha.fresh ctx.ctx.names)
      in
      let new_pred =
        mk_fun
          ctx.ctx
          [ FPatVar var ]
          (mk_bin
             Binop.And
             (mk_app old_ensures [ mk_var ctx.ctx var ])
             (mk_app ensures [ mk_var ctx.ctx var ]))
      in
      AlgoLog.show_new_ensures_predicate ~ctx p.PsiDef.reference.pvar new_pred;
      ctx >- Specifications.set_ensures p.PsiDef.reference.pvar new_pred);
    Lwt.return `CoarseningOk
;;

(** Given a counterexample, create a new term state that contains that counterexample,
  or if a term state exists for the term associated with that counterexample,
  update the term state by adding the counterexample's models.
*)
let create_or_update_lemmas_with_witness
    ~(p : PsiDef.t)
    ~(ctx : Env.env)
    ~(is_pos_witness : bool)
    (witness : witness)
    : bool Lwt.t
  =
  let update_flag = ref true in
  match
    Predicates.find_lemma_info
      ~ctx
      (witness.witness_eqn.eterm, witness.witness_eqn.esplitter)
  with
  | None ->
    ctx >- AlgoLog.announce_new_lemmas witness;
    let det, cl = term_info_of_witness ~ctx ~id:p.id ~is_pos_witness witness in
    let%lwt det' =
      if is_pos_witness
      then Lwt.return (det, cl)
      else (
        match%lwt lemma_refinement_loop ~ctx ~p det cl with
        | None ->
          update_flag := false;
          Lwt.return (det, cl)
        | Some new_cl -> Lwt.return (det, new_cl))
    in
    Predicates.add ~ctx ~key:witness.witness_eqn.eterm ~data:det';
    Lwt.return !update_flag
  | Some (det, None) ->
    let det, cl =
      match ctx >- cond_lemma_of_witness ~pos:is_pos_witness det witness with
      | Some cl -> det, cl
      | None -> term_info_of_witness ~ctx ~id:p.id ~is_pos_witness witness
    in
    let%lwt det' =
      if is_pos_witness
      then Lwt.return (det, cl)
      else (
        match%lwt lemma_refinement_loop ~ctx ~p det cl with
        | None ->
          update_flag := false;
          Lwt.return (det, cl)
        | Some new_cl -> Lwt.return (det, new_cl))
    in
    Predicates.add ~ctx ~key:witness.witness_eqn.eterm ~data:det';
    Lwt.return !update_flag
  | Some _ ->
    let change_ti det cl =
      let witness = Option.value_exn (ctx >- remap_witness_model det witness) in
      if is_pos_witness
      then Lwt.return { cl with cl_positives = witness :: cl.cl_positives }
      else (
        let cl' =
          { cl with
            cl_flag = false
          ; cl_negatives = witness :: cl.cl_negatives
          ; cl_positives = cl.cl_positives
          }
        in
        match%lwt lemma_refinement_loop ~ctx ~p det cl' with
        | None ->
          update_flag := false;
          Lwt.return cl'
        | Some new_cl -> Lwt.return new_cl)
    in
    let%lwt _ =
      Predicates.change
        ~ctx
        ~key:witness.witness_eqn.eterm
        ~split:witness.witness_eqn.esplitter
        change_ti
    in
    Lwt.return !update_flag
;;

let synthesize_lemmas
    ~(ctx : env)
    ~(p : PsiDef.t)
    synt_failure_info
    (lstate : refinement_loop_state)
    : ( (refinement_loop_state, unrealizability_witness list) Either.t
      , Sy.solver_response )
      Result.t
    Lwt.t
  =
  let _interactive_synthesis () =
    !Config.interactive_lemmas_loop
    &&
    (Log.info (fun frmt () -> Fmt.pf frmt "No luck. Try again? (Y/N)");
     match Stdio.In_channel.input_line Stdio.stdin with
     | None | Some "" | Some "N" -> false
     | Some "Y" -> true
     | _ -> false)
  in
  let update ~is_positive witness =
    Lwt_list.fold_left_s
      (fun success_flag wit ->
        let%lwt b =
          create_or_update_lemmas_with_witness ~p ~ctx ~is_pos_witness:is_positive wit
        in
        Lwt.return (success_flag && b))
      true
      witness
  in
  (*
    Example: the synt_failure_info should be a list of unrealizability counterexamples, which
    are pairs of counterexamples.
    Each counterexample can be classified as positive or negative w.r.t to the predicate p.psi_tinv.
    The lemma corresponding to a particular term should be refined to eliminate the counterexample
    (a counterexample cex is also associated to a particular term through cex.witness_eqn.eterm)
   *)
  let%lwt ( (ensures_positives, ensures_negatives)
          , (lemma_synt_positives, lemma_synt_negatives)
          , classif_failures
          , unr_witnesss )
    =
    match synt_failure_info with
    | _, Either.First _ -> failwith "There is no synt_failure_info in synthesize_lemmas."
    | _, Either.Second unrealizability_witnesss ->
      (* Forget about the specific association in pairs. *)
      let witnesss =
        List.concat_map unrealizability_witnesss ~f:(fun uc -> [ uc.ci; uc.cj ])
      in
      let%lwt classified_witnesss =
        if !Config.classify_witness
        then ctx >- LemmasInteractive.classify_witnesss_opt witnesss
        else classify_witnesss ~ctx ~p witnesss
      in
      (* Positive and negatives for the ensures predicates. *)
      let ensures_positives, ensures_negatives, _ =
        List.partition3_map ~f:witnesss_for_ensures_synt classified_witnesss
      in
      (* Positive and negatives for the requires of the target function. *)
      let lemma_synt_positives, lemma_synt_negatives, _ =
        List.partition3_map ~f:witnesss_for_lemma_synt classified_witnesss
      in
      (* Witnesses that could not be classified. *)
      let classif_failures =
        List.filter
          ~f:(fun witness ->
            match witness.witness_stat with
            | Unknown -> true
            | _ -> false)
          classified_witnesss
      in
      Lwt.return
        ( (ensures_positives, ensures_negatives)
        , (lemma_synt_positives, lemma_synt_negatives)
        , classif_failures
        , unrealizability_witnesss )
  in
  let%lwt lemma_synthesis_success =
    match ensures_negatives, lemma_synt_negatives with
    | _ :: _, _ ->
      refine_ensures_predicates
        ~ctx
        ~p
        ~neg_witnesss:ensures_negatives
        ~pos_witnesss:ensures_positives
    | _, _ :: _ ->
      AlgoLog.spurious_violates_requires (List.length lemma_synt_negatives);
      (* Update the term state by adding the positive and negative counterexamples to it. *)
      let%lwt upd_flag_1 = update ~is_positive:true lemma_synt_positives in
      let%lwt upd_flag_2 = update ~is_positive:false lemma_synt_negatives in
      Lwt.return (if upd_flag_1 && upd_flag_2 then `CoarseningOk else `CoarseningFailure)
    | [], [] ->
      (match classif_failures with
      | [] ->
        let%lwt _ = update ~is_positive:true lemma_synt_positives in
        (* lemma_synt_negatives and ensures_negatives are empty; all witnesss non spurious! *)
        AlgoLog.no_spurious_witness ();
        Lwt.return `Unrealizable
      | _ :: _ ->
        AlgoLog.witness_classification_failure ();
        Lwt.return `CoarseningFailure)
  in
  Lwt.return
    (match lemma_synthesis_success with
    | `CoarseningOk -> Ok (Either.First lstate)
    | `Unrealizable -> Ok (Either.Second unr_witnesss)
    | `CoarseningFailure ->
      (match synt_failure_info with
      | Sy.RFail, _ ->
        Log.error_msg "SyGuS solver failed to find a solution.";
        Error Sy.RFail
      | RInfeasible, _ ->
        (* The synthesis solver had answered infeasible but we couln not generate a predicate. *)
        Error Sy.RFail
      | RUnknown, _ ->
        (* In most cases if the synthesis solver does not find a solution and terminates, it will
             answer unknowns. We interpret it as "no solution can be found". *)
        Log.error_msg "SyGuS solver returned unknown.";
        Error Sy.RUnknown
      | s_resp, _ -> Error s_resp))
;;
