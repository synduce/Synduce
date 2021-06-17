open AState
open Base
open Counterexamples
open Lang
open Lang.Term
open Syguslib.Sygus
open SygusInterface
module Smt = SmtInterface
open Smtlib
open Utils

let empty_lemma = { lem_map = Map.empty (module Terms) }

let set_lemma l ~key ~lemma = { lem_map = Map.set l.lem_map ~key ~data:lemma }

let get_lemma l ~key = Map.find l.lem_map key

let add_lemmas_interactively ~(p : psi_def) (lstate : refinement_loop_state) : refinement_loop_state
    =
  let env_in_p = VarSet.of_list p.psi_reference.pargs in
  let f existing_lemmas t =
    let vars = Set.union (Analysis.free_variables t) env_in_p in
    let env = VarSet.to_env vars in
    Log.info (fun frmt () -> Fmt.pf frmt "Please provide a constraint for \"@[%a@]\"." pp_term t);
    Log.verbose (fun frmt () ->
        Fmt.pf frmt "Environment:@;@[functions %s, %s and %s@]@;and @[%a@]."
          p.psi_reference.pvar.vname p.psi_target.pvar.vname p.psi_repr.pvar.vname VarSet.pp vars);
    match Stdio.In_channel.input_line Stdio.stdin with
    | None | Some "" ->
        Log.info (fun frmt () -> Fmt.pf frmt "No additional constraint provided.");
        existing_lemmas
    | Some x -> (
        let smtterm =
          try
            let sexpr = Sexplib.Sexp.of_string x in
            Smtlib.SmtLib.smtTerm_of_sexp sexpr
          with Failure _ -> None
        in
        let pred_term = SmtInterface.term_of_smt env in
        let term x =
          match get_lemma existing_lemmas ~key:t with
          | None -> pred_term x
          | Some inv -> mk_bin Binop.And inv (pred_term x)
        in
        match smtterm with
        | None -> existing_lemmas
        | Some x -> set_lemma existing_lemmas ~key:t ~lemma:(term x))
  in
  { lstate with lemma = Set.fold ~f ~init:lstate.lemma lstate.t_set }

let ith_synth_fun index = "lemma_" ^ Int.to_string index

let synthfun_of_ctex index ctex =
  let args =
    List.map
      ~f:(fun scalar -> (scalar.vname, sort_of_rtype RType.TInt))
      (Set.elements ctex.ctex_vars)
  in
  let ret_sort = sort_of_rtype RType.TBool in
  let grammar = Grammars.generate_grammar ~guess:None ~bools:true OpSet.empty args ret_sort in
  CSynthFun (ith_synth_fun index, args, ret_sort, grammar)

let constraint_of_ctex index ctex =
  let var_list = List.map ~f:(fun t -> sygus_of_term t) (Map.data ctex.ctex_model) in
  (* TODO: check that order of terms in Map.data model is the same as order of arguments to xi *)
  CConstraint (SyApp (IdSimple "not", [ SyApp (IdSimple (ith_synth_fun index), var_list) ]))

let log_soln s vs t =
  Log.verbose (fun frmt () ->
      Fmt.pf frmt "Lemma candidate: \"%s %s = @[%a@]\"." s
        (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vs))
        pp_term t)

let handle_lemma_synth_response (resp : solver_response option) =
  let parse_synth_fun (fname, fargs, _, fbody) =
    let args =
      let f (varname, sort) = Variable.mk ~t:(rtype_of_sort sort) varname in
      List.map ~f fargs
    in
    let local_vars = VarSet.of_list args in
    let body, _ = infer_type (term_of_sygus (VarSet.to_env local_vars) fbody) in
    (fname, args, body)
  in
  match resp with
  | Some (RSuccess resps) ->
      let soln = List.map ~f:parse_synth_fun resps in
      let _ = List.iter ~f:(fun (s, vs, t) -> log_soln s vs t) soln in
      Some soln
  | Some RInfeasible | Some RFail | Some RUnknown | None -> None

let verify_lemma_candidate _psi _lemma_candidates _negative_ctexs (terms : (string * term) list) =
  Log.info (fun f () -> Fmt.(pf f "Checking lemma candidate..."));
  let _start_time = Unix.gettimeofday () in
  let solver = Solvers.make_cvc4_solver () in
  let _ = Solvers.set_logic solver "ALL" in
  let _lemma_names =
    VarSet.of_list
      (List.map ~f:(fun (synth_name, _term) -> Variable.mk ~t:(Some RType.TBool) synth_name) terms)
  in
  Solvers.load_min_max_defs solver;
  (* TODO:  check if TInv => lemma candidates *)
  (* TODO: If not, get counterexample *)
  let _ =
    match Solvers.check_sat solver with
    | Sat -> (
        match Solvers.get_model solver with
        | SExps s ->
            let _model = Smt.model_to_constmap (SExps s) in
            ()
        | _ -> ())
    | _ -> ()
  in
  Solvers.close_solver solver

let classify_ctexs_opt ~(p : psi_def) ctexs =
  if !Config.classify_ctex then
    let f (p, n) ctex =
      Log.info (fun frmt () ->
          Fmt.(pf frmt "Classify this counterexample: %a (P/N)" (box pp_ctex) ctex));
      match Stdio.In_channel.input_line Stdio.stdin with
      | Some "N" -> (p, ctex :: n)
      | _ -> (ctex :: p, n)
    in
    List.fold ~f ~init:([], []) ctexs
  else classify_ctexs ~p ctexs

let synthesize_lemmas ~(p : psi_def) synt_failure_info (lstate : refinement_loop_state) :
    (refinement_loop_state, solver_response) Result.t =
  (*
    Example: the synt_failure_info should be a list of unrealizability counterexamples, which
    are pairs of counterexamples.
    Each counterexample can be classfied as positive or negative w.r.t to the predicate p.psi_tinv.
    The lemma corresponding to a particular term should be refined to eliminate the counterexample
    (a counterexample cex is also asscociated to a particular term through cex.ctex_eqn.eterm)
   *)
  let lemma_candidates, negative_ctex =
    match synt_failure_info with
    | _, Either.Second unrealizability_ctexs ->
        (* Forget about the specific association in pairs. *)
        let ctexs = List.concat_map unrealizability_ctexs ~f:(fun uc -> [ uc.ci; uc.cj ]) in
        (* Classify in negative and positive cexs. *)
        let _positive_ctexs, negative_ctexs = classify_ctexs_opt ~p ctexs in
        let set_logic = CSetLogic "DTLIA" in
        (* TODO: How to choose logic? *)
        let synth_objs = List.mapi ~f:synthfun_of_ctex negative_ctexs in
        let constraints = List.mapi ~f:constraint_of_ctex negative_ctexs in
        let extra_defs = [ max_definition; min_definition ] in
        let commands = set_logic :: (extra_defs @ synth_objs @ constraints @ [ CCheckSynth ]) in
        ( handle_lemma_synth_response (Syguslib.Solvers.SygusSolver.solve_commands commands),
          negative_ctexs )
    | _ -> failwith "There is no synt_failure_info in synthesize_lemmas."
  in
  let terms = List.mapi ~f:(fun i ctex -> (ith_synth_fun i, ctex.ctex_eqn.eterm)) negative_ctex in
  let _ = verify_lemma_candidate p lemma_candidates negative_ctex terms in
  if
    !Config.interactive_lemmas_loop
    &&
    (Log.info (fun frmt () -> Fmt.pf frmt "No luck. Try again? (Y/N)");
     match Stdio.In_channel.input_line Stdio.stdin with
     | None | Some "" | Some "N" -> false
     | Some "Y" -> true
     | _ -> false)
  then Ok lstate
  else
    match synt_failure_info with
    | RFail, _ ->
        Log.error_msg "SyGuS solver failed to find a solution.";
        Error RFail
    | RInfeasible, _ ->
        (* Rare - but the synthesis solver can answer "infeasible", in which case it can give
           counterexamples. *)
        Log.info
          Fmt.(
            fun frmt () ->
              pf frmt "@[<hov 2>This problem has no solution. Counterexample set:@;%a@]"
                (list ~sep:sp pp_term) (Set.elements lstate.t_set));
        Error RInfeasible
    | RUnknown, _ ->
        (* In most cases if the synthesis solver does not find a solution and terminates, it will
           answer unknowns. We interpret it as "no solution can be found". *)
        Log.error_msg "SyGuS solver returned unknown.";
        Error RUnknown
    | s_resp, _ -> Error s_resp
