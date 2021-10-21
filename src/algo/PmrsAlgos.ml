open AState
open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils
open Option.Let_syntax

let rec refinement_loop (p : psi_def) (lstate : refinement_loop_state) =
  Int.incr refinement_steps;
  (* Output status information before entering process. *)
  let elapsed = Stats.get_glob_elapsed () in
  Log.info
    Fmt.(
      fun frmt () ->
        (styled
           (`Fg `Black)
           (styled
              (`Bg (`Hi `Green))
              (fun frmt (i, j) -> pf frmt "\t\t Refinement step %i - %i " i j)))
          frmt
          (!refinement_steps, !secondary_refinement_steps));
  (if not !Config.info
  then
    Fmt.(
      pf
        stdout
        "%i,%3.3f,%3.3f,%i,%i@."
        !refinement_steps
        !Stats.verif_time
        elapsed
        (Set.length lstate.t_set)
        (Set.length lstate.u_set)));
  Log.debug_msg
    Fmt.(
      str
        "Start refinement loop with %i terms in T, %i terms in U."
        (Set.length lstate.t_set)
        (Set.length lstate.u_set));
  let lstate =
    if !Config.interactive_lemmas
    then Lemmas.add_lemmas_interactively ~p lstate
    else lstate
  in
  (* First, generate the set of constraints corresponding to the set of terms t_set. *)
  let eqns, lifting =
    Equations.make ~p ~term_state:lstate.term_state ~lifting:lstate.lifting lstate.t_set
  in
  (* The solve the set of constraints. *)
  let s_resp, solution = Equations.solve ~p eqns in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (* Synthesis has succeeded, now we need to verify the solution. *)
    (try
       (* The solution is verified with a bounded check.  *)
       let check_r = Verify.check_solution ~p lstate sol in
       match check_r with
       | Some (t_set, u_set) ->
         (* If check_r is some new set of MR-terms t_set, and terms u_set, this means
               verification failed. The generalized counterexamples have been added to new_t_set,
               which is also a superset of t_set.
            *)
         Log.debug (fun frmt () ->
             Fmt.(
               pf
                 frmt
                 "@[<hov 2>Counterexample terms:@;@[<hov 2>%a@]"
                 (list ~sep:comma pp_term)
                 (Set.elements (Set.diff t_set lstate.t_set))));
         secondary_refinement_steps := 0;
         (* Continue looping with the new sets. *)
         refinement_loop p { lstate with t_set; u_set; lifting }
       | None ->
         (* This case happens when verification succeeded.
               Store the equation system, return the solution. *)
         AState.solved_eqn_system := Some eqns;
         Log.print_ok ();
         Ok { soln_rec_scheme = p.psi_target; soln_implems = sol }
     with
    | _ ->
      (* A failure during the bounded check is an error. *)
      Log.error_msg "Solution is incorrect, and synthesized and verifier disagree on why.";
      Log.error_msg "Try without grammar optimizations (option --no-gropt).";
      Error RFail)
  | _ as synt_failure_info ->
    (* On synthesis failure, start by trying to synthesize lemmas. *)
    (match Lemmas.synthesize_lemmas ~p synt_failure_info lstate with
    | Ok new_lstate ->
      Int.decr refinement_steps;
      Int.incr secondary_refinement_steps;
      refinement_loop p new_lstate
    | Error _synt_failure
      when !Config.attempt_lifting
           && Lifting.lifting_count p < !Config.max_lifting_attempts ->
      (* If all no counterexample is spurious, lemma synthesis fails, we need lifting. *)
      (match Lifting.scalar ~p lstate synt_failure_info with
      | Ok (p', lstate') ->
        Int.decr refinement_steps;
        Int.incr secondary_refinement_steps;
        Lifting.msg_lifting ();
        refinement_loop p' lstate'
      | Error r' -> Error r')
    | _ -> Error RFail)
;;

let psi (p : psi_def) =
  (* Initialize sets with the most general terms. *)
  let t_set, u_set =
    if !Config.simple_init
    then (
      let x0 = mk_var (Variable.mk ~t:(Some !AState._theta) (Alpha.fresh ())) in
      let s = TermSet.of_list (Analysis.expand_once x0) in
      Set.partition_tf ~f:(Expand.is_mr_all p) s)
    else (
      let init_set = MGT.most_general_terms p.psi_target in
      Set.fold init_set ~init:(TermSet.empty, TermSet.empty) ~f:(fun (t, u) mgt ->
          let t', u' = Expand.to_maximally_reducible p mgt in
          Set.union t t', Set.union u u'))
  in
  Log.debug (fun frmt () ->
      Fmt.(pf frmt "@[<hov 2>INIT = %a@]" (list ~sep:comma pp_term) (Set.elements t_set)));
  if Set.is_empty t_set
  then (
    Log.error_msg "Empty set of terms for equation system.";
    failwith "Cannot solve problem.")
  else (
    refinement_steps := 0;
    refinement_loop
      p
      { t_set
      ; u_set
      ; term_state = Lemmas.empty_term_state
      ; lifting = Lifting.empty_lifting
      })
;;

(* ============================================================================================= *)
(*                                                 MAIN ENTRY POINTS                             *)
(* ============================================================================================= *)

let no_synth () = failwith "No synthesis objective found."

let sync_args p : psi_def =
  let subs =
    match List.zip p.psi_reference.pargs p.psi_target.pargs with
    | Unequal_lengths ->
      failwith
        "Reference and target recursion scheme must have the same number of parameters."
    | Ok var_subs -> List.map ~f:(fun (v1, v2) -> mk_var v2, mk_var v1) var_subs
  in
  let target' =
    PMRS.subst_rule_rhs ~p:{ p.psi_target with pargs = p.psi_reference.pargs } subs
  in
  { p with psi_target = target' }
;;

(**
  [solve_problem (Some (target, reference, representation))] solves the synthesis problem
  associated with the target function named [target], the reference function named
  [reference] and the representation function named [representation] that have
  been parsed in the file.
  If None is passed as argument, the default values are ("target", "spec", "repr").
  If the functions cannot be found, it will exit the program.
*)
let solve_problem
    (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    : psi_def * (soln, solver_response) Result.t
  =
  let target_fname, spec_fname, repr_fname =
    match psi_comps with
    | Some names -> names
    | None ->
      Utils.Log.debug_msg "Using default names.";
      "target", "spec", "repr"
  in
  (* Representation function. *)
  let repr, theta_to_tau =
    match Map.find pmrs repr_fname with
    | Some pmrs -> Either.First pmrs, Variable.vtype_or_new pmrs.pmain_symb
    | None ->
      let reprs =
        Hashtbl.filter
          ~f:(fun (v, _, _, _) -> String.(v.vname = repr_fname))
          Term._globals
      in
      (match Hashtbl.choose reprs with
      | Some (_, (f, a, _, b)) -> Either.Second (f, a, b), Variable.vtype_or_new f
      (* No repr specified: assume identity. *)
      | None ->
        let x = Variable.mk "x" in
        let xt = Variable.vtype_or_new x in
        let repr_fun = Variable.mk ~t:(Some (TFun (xt, xt))) repr_fname in
        Either.Second (repr_fun, [ FPatVar x ], mk_var x), RType.TFun (xt, xt))
  in
  (* Reference function. *)
  let reference_f, tau =
    match Map.find pmrs spec_fname with
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
  let target_f, theta =
    let target_f =
      match Map.find pmrs target_fname with
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
  (match RType.fun_typ_unpack theta_to_tau with
  | [ theta' ], tau' -> PMRS.unify_two_with_update (theta, theta') (tau, tau')
  | _ ->
    Log.error_msg "Representation function should be a function.";
    Log.fatal ());
  Term.(
    let reference_out = Variable.vtype_or_new reference_f.pmain_symb in
    let target_out = Variable.vtype_or_new target_f.pmain_symb in
    Log.debug_msg
      Fmt.(str "ɑ : unify %a and %a" RType.pp reference_out RType.pp target_out);
    match reference_out, target_out with
    | TFun (_, tout), TFun (_, tout') ->
      (match RType.unify_one tout tout' with
      | Ok subs -> Variable.update_var_types (RType.mkv subs)
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
    | Either.First pmrs -> Either.First (PMRS.infer_pmrs_types pmrs)
    | Either.Second (f, a, b) ->
      let b', _ = Term.infer_type b in
      Either.Second (f, a, b')
  in
  let target_f = PMRS.infer_pmrs_types target_f in
  let reference_f = PMRS.infer_pmrs_types reference_f in
  let args_t = target_f.pinput_typ in
  let t_out = reference_f.poutput_typ in
  let repr_pmrs =
    match repr with
    | Either.First p -> p
    | Either.Second (f, a, b) -> PMRS.func_to_pmrs f a b
  in
  let tinv_pmrs =
    let%bind spec = Specifications.get_spec target_f.pvar in
    let%bind t = spec.requires in
    match t.tkind with
    | TVar func_var -> Hashtbl.find PMRS._globals func_var.vid
    | _ -> None
  in
  let problem =
    sync_args
      { psi_target = target_f
      ; psi_reference = reference_f
      ; psi_repr = repr_pmrs
      ; psi_tinv = tinv_pmrs
      ; psi_repr_is_identity = Reduce.is_identity repr_pmrs
      ; psi_lifting = []
      }
  in
  (* Print summary information about the problem, before solving.*)
  Log.info
    Fmt.(
      fun fmt () ->
        pf
          fmt
          " Ψ (%a) := ∀ x : %a. (%s o %s)(x) = %s(x)"
          (list ~sep:comma Term.Variable.pp)
          (Set.elements target_f.psyntobjs)
          (list ~sep:sp RType.pp)
          args_t
          spec_fname
          repr_fname
          target_fname);
  (* Print reference function. *)
  Log.info Fmt.(fun fmt () -> pf fmt "%a" (box PMRS.pp) problem.psi_reference);
  (* Print target recursion skeleton. *)
  Log.info Fmt.(fun fmt () -> pf fmt "%a" (box PMRS.pp) problem.psi_target);
  (* Print representation function. *)
  Log.info
    Fmt.(
      fun fmt () ->
        match repr with
        | Either.First pmrs -> pf fmt "%a" PMRS.pp pmrs
        | Either.Second (fv, args, body) ->
          pf
            fmt
            "%s(%a) = %a"
            fv.vname
            (list ~sep:comma Term.pp_fpattern)
            args
            Term.pp_term
            body);
  Log.verbose Specifications.dump_all;
  (* Print the condition on the reference function's input, if there is one. *)
  (match problem.psi_tinv with
  | Some tinv -> Log.info (fun formt () -> Fmt.(pf formt "%a" PMRS.pp tinv))
  | None -> ());
  (* Set global information. *)
  AState._tau := tau;
  AState._theta := theta;
  AState._alpha := t_out;
  AState._span := List.length (Analysis.terms_of_max_depth 1 theta);
  AState.refinement_steps := 0;
  (* Solve the problem. *)
  ( problem
  , if !Config.use_acegis
    then Baselines.algo_acegis problem
    else if !Config.use_ccegis
    then Baselines.algo_ccegis problem
    else psi problem )
;;
