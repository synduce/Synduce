open Base
open Common
open Counterexamples
open Env
open Elim
open Lang
open LemmaVerif
open Term
open ProblemDefs
open Rewriter
open SmtInterface
open SygusInterface
open Utils
module S = Smtlib.SmtLib
module Sy = Syguslib.Sygus
module Sm = Syguslib.Semantic

let empty_lemmas : lemmas = Map.empty (module KeyedTerms)

(* ============================================================================================= *)
(*                                  Creating and updating term states                            *)
(* ============================================================================================= *)

let lemmas_of_context ~(ctx : Context.t) ~(is_pos_ctex : bool) (ctex : ctex) : term_info =
  let scalar_vars = Map.keys ctex.ctex_model in
  let input_args_t = List.map ~f:(Variable.vtype_or_new ctx) scalar_vars in
  let lemma_f =
    Variable.mk
      ctx
      ~t:(Some (RType.fun_typ_pack input_args_t TBool))
      (Alpha.fresh ~s:"lemma" ctx.names)
  in
  { term = ctex.ctex_eqn.eterm
  ; splitter = ctex.ctex_eqn.esplitter
  ; lemmas = []
  ; lemma = lemma_f
  ; lemma_candidate = None
  ; negative_ctexs = (if is_pos_ctex then [] else [ ctex ])
  ; positive_ctexs = (if is_pos_ctex then [ ctex ] else [])
  ; recurs_elim = ctex.ctex_eqn.eelim
  ; scalar_vars = Map.keys ctex.ctex_model
  ; current_preconds = ctex.ctex_eqn.eprecond
  }
;;

(** Given a counterexample, create a new term state that contains that counterexample,
  or if a term state exists for the term associated with that counterexample,
  update the term state by adding the counterexample's models.
*)
let create_or_update_lemmas_with_ctex
    ~(ctx : Context.t)
    ~(is_pos_ctex : bool)
    (ts : lemmas)
    (ctex : ctex)
    : lemmas
  =
  match Map.find ts (ctex.ctex_eqn.eterm, ctex.ctex_eqn.esplitter) with
  | None ->
    AlgoLog.announce_new_lemmas ~ctx ctex;
    Map.add_exn
      ~key:(ctex.ctex_eqn.eterm, ctex.ctex_eqn.esplitter)
      ~data:(lemmas_of_context ~ctx ~is_pos_ctex ctex)
      ts
  | Some _ ->
    Map.update ts (ctex.ctex_eqn.eterm, ctex.ctex_eqn.esplitter) ~f:(fun maybe_det ->
        match maybe_det with
        | None -> failwith "Term detail does not exist."
        | Some det ->
          if is_pos_ctex
          then { det with positive_ctexs = ctex :: det.positive_ctexs }
          else
            { det with
              current_preconds =
                (match ctex.ctex_eqn.eprecond with
                | None -> None
                | Some pre ->
                  let pre' =
                    substitution
                      (subs_from_elim_to_elim ~ctx det.recurs_elim ctex.ctex_eqn.eelim)
                      pre
                  in
                  Some pre')
            ; negative_ctexs = ctex :: det.negative_ctexs
            ; positive_ctexs = det.positive_ctexs
            })
;;

(* ============================================================================================= *)
(*                                  Lemma synthesis    functions                                 *)
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
    Option.(List.findi ~f:(fun _ a -> Variable.equal a var) det.scalar_vars >>| fst)
  in
  let _tuple_of_args args =
    VarSet.union_list
      (List.map
         ~f:(fun arg ->
           Analysis.free_variables
             ~ctx
             ~include_functions:false
             (substitution det.recurs_elim arg))
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
let synthfun_of_det
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (det : term_info)
    : (Sy.command * string) list
  =
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
    let grammar =
      Grammars.generate_grammar
        ~ctx
        ~short_predicate:t
        ~guess
        ~bools:true
        opset
        det.scalar_vars
        RType.TBool
    in
    let logic = logic_of_operators opset in
    mk_synthinv ~ctx det.lemma.vname det.scalar_vars grammar, logic
  in
  if !Config.Optims.make_partial_lemma_sketches
  then (
    let skeleton_guess =
      match p.tinv with
      | Some tinv ->
        skeleton_of_tinv ~fctx ~ctx det (Reduce.reduce_pmrs ~ctx ~fctx tinv det.term)
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

let convert_term_rec_to_ctex_rec
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (det : term_info)
    (ctex : ctex)
    (name : string)
    : string
  =
  let rec g recvar lst =
    match lst with
    | [] ->
      failwith
        (Fmt.str "Could not find name %s in the ctex's recursion elimination." name)
    | (a, b) :: tl ->
      let i = term_var_string ~ctx b in
      let l = term_var_string ~ctx a in
      if String.(equal l recvar) then i else g recvar tl
  in
  let rec h recvar n elim =
    match elim with
    | [] ->
      failwith
        (Fmt.str "Could not find ctex rec elim entry for tuple entry named %s." name)
    | (a, b) :: tl ->
      let l = term_var_string ~ctx a in
      if String.(equal l recvar)
      then (
        match b.tkind with
        | TTup vars -> term_var_string ~ctx (List.nth_exn vars n)
        | _ ->
          failwith
            Fmt.(
              str "Cannot get tuple entry %s in ctex rec elim; %s is not a tuple.)" name l))
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
               if String.(equal i name) then Some (h l n ctex.ctex_eqn.eelim) else None)
         with
        | Some s -> s
        | None -> f tl)
      | _ ->
        let i = term_var_string ~ctx b in
        let l = term_var_string ~ctx a in
        if String.(equal i name) then g l ctex.ctex_eqn.eelim else f tl)
  in
  match
    VarSet.find_by_name
      (Set.union
         (Analysis.free_variables ~ctx det.term)
         (Set.union (VarSet.of_list p.PsiDef.reference.pargs) ctex.ctex_vars))
      name
  with
  | None -> f det.recurs_elim
  | Some _ -> name
;;

let ctex_model_to_args
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (det : term_info)
    (params : (string * Sy.sygus_sort) list)
    ctex
    : Sy.sygus_term list
  =
  List.map params ~f:(fun (param_name, _) ->
      match
        let name = convert_term_rec_to_ctex_rec ~ctx ~p det ctex param_name in
        Map.find
          ctex.ctex_model
          (match
             VarSet.find_by_name
               (Set.union
                  (* Don't include functions, we won't get a model for them in CVC5. *)
                  (Analysis.free_variables ~ctx ~include_functions:false det.term)
                  (Set.union (VarSet.of_list p.PsiDef.reference.pargs) ctex.ctex_vars))
               name
           with
          | None ->
            failwith
              (Fmt.str
                 "Failed to extract argument list from ctex model (%s unknown)."
                 name)
          | Some v -> v)
      with
      | None ->
        Log.error Fmt.(fun fmt () -> pf fmt "I was looking for %s" param_name);
        Log.error Fmt.(fun fmt () -> pf fmt "The ctex: %a" (Pretty.pp_ctex ~ctx) ctex);
        failwith "Failed to extract argument list from ctex model."
      | Some t -> sygus_of_term ~ctx t)
;;

let constraint_of_neg_ctex ~ctx (det : term_info) ctex =
  let neg_constraint =
    mk_un Not (mk_app (mk_var ctx det.lemma) (Map.data ctex.ctex_model))
  in
  Sy.mk_c_constraint (sygus_of_term ~ctx neg_constraint)
;;

let constraint_of_pos_ctex ~ctx (det : term_info) ctex =
  let pos_constraint = mk_app (mk_var ctx det.lemma) (Map.data ctex.ctex_model) in
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
    : term list option
  =
  let parse_synth_fun (_, _, _, fbody) =
    let body, _ =
      infer_type
        ctx
        (term_of_sygus ~fctx ~ctx (VarSet.to_env (VarSet.of_list ti.scalar_vars)) fbody)
    in
    body
  in
  match
    Lwt_main.run
      (Lwt.wakeup resolver 0;
       task)
  with
  | Some (RSuccess resps) ->
    let soln = List.map ~f:parse_synth_fun resps in
    let _ = List.iter ~f:(fun t -> log_soln ~ctx ti.lemma.vname ti.scalar_vars t) soln in
    Some soln
  | Some RInfeasible | Some RFail | Some RUnknown | None -> None
;;

let get_lemma ~(ctx : Context.t) ~(p : PsiDef.t) ~(key : term) (ts : lemmas) : term option
  =
  let term_info_to_lemma det =
    (* Recursion elimination substitutions *)
    let subst =
      List.concat_map
        ~f:(fun (t1, t2) ->
          let frt1 = mk_f_compose_r_main ~ctx ~p t1 in
          match t2.tkind with
          | TTup t2s -> List.mapi t2s ~f:(fun i t2_i -> t2_i, mk_sel ctx frt1 i)
          | _ -> [ t2, frt1 ])
        det.recurs_elim
    in
    let f lem = Term.substitution subst lem in
    Option.map ~f:(simplify_term ~ctx) (mk_assoc Binop.And (List.map ~f det.lemmas))
  in
  match
    List.unzip (Map.to_alist (Map.filter_keys ~f:(fun (k, _) -> Terms.equal k key) ts))
  with
  | _, [] -> None
  | _, dets ->
    (match List.filter_opt (List.map ~f:term_info_to_lemma dets) with
    | [] -> None
    | [ a ] -> Some a
    | _ as conds -> mk_assoc Binop.And conds)
;;

let parse_positive_example_solver_model
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    response
    (det : term_info)
  =
  match response with
  | S.SExps s ->
    let model = model_to_constmap ~ctx ~fctx (SExps s) in
    let m, _ =
      Map.partitioni_tf
        ~f:(fun ~key ~data:_ ->
          Option.is_some (VarSet.find_by_name (VarSet.of_list det.scalar_vars) key))
        model
    in
    (* Remap the names to ids of the original variables in m' *)
    [ ({ (placeholder_ctex det) with
         ctex_model =
           Map.fold
             ~init:VarMap.empty
             ~f:(fun ~key ~data acc ->
               match VarSet.find_by_name (VarSet.of_list det.scalar_vars) key with
               | None ->
                 Log.info (fun f () -> Fmt.(pf f "Could not find by name %s" key));
                 acc
               | Some var -> Map.set ~data acc ~key:var)
             m
       }
        : ctex)
    ]
  | _ ->
    failwith
      "Parse model failure: Positive example cannot be found during lemma refinement."
;;

let synthesize_new_lemma ~(ctx : env) ~(p : PsiDef.t) (det : term_info) : term option =
  let with_synth_obj i synth_obj logic =
    ctx >- AlgoLog.announce_new_lemma_synthesis i det;
    let neg_constraints =
      List.map ~f:(ctx >- constraint_of_neg_ctex det) det.negative_ctexs
    in
    let pos_constraints =
      List.map ~f:(ctx >- constraint_of_pos_ctex det) det.positive_ctexs
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
    match
      ctx
      >>- handle_lemma_synth_response
            det
            (SygusInterface.SygusSolver.solve_commands commands)
    with
    | None -> None
    | Some solns -> List.nth solns 0
  in
  match ctx >>- synthfun_of_det ~p det with
  | [ (synth_obj, logic) ] -> with_synth_obj 0 synth_obj logic
  | obj_choices ->
    let lwt_tasks =
      List.mapi obj_choices ~f:(fun i (synth_obj, logic) ->
          Lwt.task ()
          |> fun (t, r) -> Lwt.map (fun _ -> with_synth_obj i synth_obj logic) t, r)
    in
    Lwt_main.run
      (Lwt.pick
         (List.map
            ~f:(fun (t, r) ->
              Lwt.wakeup r 0;
              t)
            lwt_tasks))
;;

(* ============================================================================================= *)
(*                                  Main entry points                                            *)
(* ============================================================================================= *)

let rec lemma_refinement_loop ~(ctx : env) ~(p : PsiDef.t) (det : term_info)
    : term_info option
  =
  match synthesize_new_lemma ~ctx ~p det with
  | None ->
    Log.debug_msg "Lemma synthesis failure.";
    None
  | Some lemma_term ->
    if !Config.interactive_check_lemma
    then
      ctx
      >- LemmasInteractive.interactive_check_lemma
           (lemma_refinement_loop ~ctx ~p)
           det.lemma.vname
           det.scalar_vars
           lemma_term
           det
    else (
      match
        ctx >>- verify_lemma_candidate ~p { det with lemma_candidate = Some lemma_term }
      with
      | vmethod, Unsat ->
        let lemma =
          match det.splitter with
          | None -> lemma_term
          | Some pre -> Terms.(pre => lemma_term)
        in
        ctx >- AlgoLog.lemma_proved_correct vmethod det lemma;
        Some { det with lemma_candidate = None; lemmas = lemma :: det.lemmas }
      | vmethod, S.SExps x ->
        AlgoLog.lemma_not_proved_correct vmethod;
        let new_positive_ctexs =
          ctx >>- parse_positive_example_solver_model (S.SExps x) det
        in
        List.iter
          ~f:(fun ctex ->
            Log.verbose (fun f () ->
                Fmt.(
                  pf f "Found a positive example: %a" (box (ctx >- Pretty.pp_ctex)) ctex)))
          new_positive_ctexs;
        lemma_refinement_loop
          ~ctx
          ~p
          { det with positive_ctexs = det.positive_ctexs @ new_positive_ctexs }
      | _, Sat ->
        Log.error_msg "Lemma verification returned Sat. This is unexpected.";
        None
      | _, Unknown ->
        Log.error_msg "Lemma verification returned Unknown.";
        None
      | _ ->
        Log.error_msg "Lemma verification is indeterminate.";
        None)
;;

(** Partitioning function to partitiion a list into (a,b,c) where a are
  examples that satisfy the invariant,
  b are examples that do not satisfy the invariant,
  c are examples that are spurious for other reasons.
*)
let ctexs_for_lemma_synt ctex =
  match ctex.ctex_stat with
  | Valid -> `Fst ctex
  | Spurious causes ->
    if Caml.List.mem ViolatesTargetRequires causes then `Snd ctex else `Trd ctex
  | _ -> `Trd ctex
;;

(** Partitioning function to partitiion a list into (a,b,c) where a are
  examples that are not in the reference function's image,
  b are examples that are in the reference function's image,
  c are examples that are spurious for other reasons.
*)
let ctexs_for_ensures_synt ctex =
  match ctex.ctex_stat with
  | Valid -> `Fst ctex
  | Spurious causes ->
    if Caml.List.mem NotInReferenceImage causes then `Snd ctex else `Trd ctex
  | _ -> `Trd ctex
;;

let refine_ensures_predicates
    ~(ctx : env)
    ~(p : PsiDef.t)
    ~(neg_ctexs : ctex list)
    ~(pos_ctexs : ctex list)
    (lstate : refinement_loop_state)
    : lemmas * [ `CoarseningOk | `CoarseningFailure | `Unrealizable ]
  =
  Log.info
    Fmt.(
      fun fmt () ->
        pf fmt "%i counterexamples violate image assumption." (List.length neg_ctexs));
  let maybe_pred = ImagePredicates.synthesize ~ctx ~p pos_ctexs neg_ctexs [] in
  match maybe_pred with
  | None -> lstate.lemmas, `CoarseningFailure
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
    lstate.lemmas, `CoarseningOk
;;

let synthesize_lemmas
    ~(ctx : env)
    ~(p : PsiDef.t)
    synt_failure_info
    (lstate : refinement_loop_state)
    : ( (refinement_loop_state, unrealizability_ctex list) Either.t, Sy.solver_response
    ) Result.t
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
  let update is_positive ctexs ts =
    List.fold
      ctexs
      ~init:ts
      ~f:(ctx >- create_or_update_lemmas_with_ctex ~is_pos_ctex:is_positive)
  in
  (*
    Example: the synt_failure_info should be a list of unrealizability counterexamples, which
    are pairs of counterexamples.
    Each counterexample can be classified as positive or negative w.r.t to the predicate p.psi_tinv.
    The lemma corresponding to a particular term should be refined to eliminate the counterexample
    (a counterexample cex is also associated to a particular term through cex.ctex_eqn.eterm)
   *)
  let ( (ensures_positives, ensures_negatives)
      , (lemma_synt_positives, lemma_synt_negatives)
      , unr_ctexs )
    =
    match synt_failure_info with
    | _, Either.First _ -> failwith "There is no synt_failure_info in synthesize_lemmas."
    | _, Either.Second unrealizability_ctexs ->
      (* Forget about the specific association in pairs. *)
      let ctexs = List.concat_map unrealizability_ctexs ~f:(fun uc -> [ uc.ci; uc.cj ]) in
      let classified_ctexs =
        if !Config.classify_ctex
        then ctx >- LemmasInteractive.classify_ctexs_opt ctexs
        else classify_ctexs ~ctx ~p ctexs
      in
      (* Positive and negatives for the ensures predicates. *)
      let ensures_positives, ensures_negatives, _ =
        List.partition3_map ~f:ctexs_for_ensures_synt classified_ctexs
      in
      (* Positive and negatives for the requires of the target function. *)
      let lemma_synt_positives, lemma_synt_negatives, _ =
        List.partition3_map ~f:ctexs_for_lemma_synt classified_ctexs
      in
      ( (ensures_positives, ensures_negatives)
      , (lemma_synt_positives, lemma_synt_negatives)
      , unrealizability_ctexs )
  in
  let new_state, lemma_synthesis_success =
    match ensures_negatives, lemma_synt_negatives with
    | _ :: _, _ ->
      refine_ensures_predicates
        ~ctx
        ~p
        ~neg_ctexs:ensures_negatives
        ~pos_ctexs:ensures_positives
        lstate
    | _, _ :: _ ->
      (* Update the term state by adding the positive and negative counterexamples to it. *)
      let ts : lemmas =
        lstate.lemmas
        |> update false lemma_synt_negatives
        |> update true lemma_synt_positives
      in
      AlgoLog.spurious_violates_requires (List.length lemma_synt_negatives);
      let new_ts, success =
        Map.fold
          ts
          ~init:(Map.empty (module KeyedTerms), true)
          ~f:(fun ~key ~data:det (acc, status) ->
            if ctx >- Analysis.is_bounded det.term
            then acc, status (* Skip lemma synth for bounded terms. *)
            else if not status
            then acc, status
            else (
              match lemma_refinement_loop ~ctx ~p det with
              | None -> acc, false
              | Some det -> Map.add_exn ~key ~data:det acc, status))
      in
      new_ts, if success then `CoarseningOk else `CoarseningFailure
    | [], [] ->
      let ts = lstate.lemmas |> update true lemma_synt_positives in
      (* lemma_synt_negatives and ensures_negatives are empty; all ctexs non spurious! *)
      AlgoLog.no_spurious_ctex ();
      ts, `Unrealizable
  in
  match lemma_synthesis_success with
  | `CoarseningOk -> Ok (Either.First { lstate with lemmas = new_state })
  | `Unrealizable -> Ok (Either.Second unr_ctexs)
  | `CoarseningFailure ->
    (match synt_failure_info with
    | Sy.RFail, _ ->
      Log.error_msg "SyGuS solver failed to find a solution.";
      Error RFail
    | RInfeasible, _ ->
      (* Rare - but the synthesis solver can answer "infeasible", in which case it can give
             counterexamples. *)
      ctx >- AlgoLog.print_infeasible_message lstate.t_set;
      Ok (Either.Second unr_ctexs)
    | RUnknown, _ ->
      (* In most cases if the synthesis solver does not find a solution and terminates, it will
             answer unknowns. We interpret it as "no solution can be found". *)
      Log.error_msg "SyGuS solver returned unknown.";
      Error RUnknown
    | s_resp, _ -> Error s_resp)
;;
