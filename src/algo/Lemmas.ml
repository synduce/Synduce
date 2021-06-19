open AState
open Base
open Counterexamples
open Lang
open Lang.Term
open Syguslib.Sygus
open SygusInterface
open Utils
open Smtlib
module Smt = SmtInterface
module S = Smtlib.SmtLib

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

let smt_of_lemma_validity ~(p : psi_def) (lemma_name, lemma_args, _lemma_body) (ctex : ctex option)
    =
  match ctex with
  | None -> []
  | Some ctex ->
      (* TODO: complete this function *)
      (* General form: assert not forall (scalar vars in ctex + rec type vars in term) (if (TInv(term) and (recursion elim equations) and (relevant conditions)) then lemma (scalar vars in ctex)) *)
      let mk_sort maybe_rtype =
        match maybe_rtype with
        | None -> S.mk_int_sort
        | Some rtype -> (
            match RType.base_name rtype with
            | None -> S.mk_int_sort
            | Some name -> S.mk_simple_sort name)
      in
      let quants =
        List.map
          ~f:(fun var -> (S.SSimple var.vname, mk_sort (Variable.vtype var)))
          (Set.elements (Set.union ctex.ctex_vars (Analysis.free_variables ctex.ctex_eqn.eterm)))
      in
      let tinv_app =
        match p.psi_tinv with
        | None -> failwith "No TInv has been specified. Cannot check lemma validity."
        | Some pmrs -> S.mk_simple_app pmrs.pvar.vname [ Smt.smt_of_term ctex.ctex_eqn.eterm ]
      in
      let rec_elim_eqns =
        List.fold ~init:S.mk_true
          ~f:(fun acc (t1, t2) ->
            let applic =
              S.mk_simple_app p.psi_reference.pvar.vname
                [
                  (if p.psi_repr_is_identity then Smt.smt_of_term t1
                  else S.mk_simple_app p.psi_repr.pvar.vname [ Smt.smt_of_term t1 ]);
                ]
            in
            let eqn = S.mk_eq applic (Smt.smt_of_term t2) in
            S.mk_and acc eqn)
          ctex.ctex_eqn.eelim
      in
      let if_condition = S.mk_and tinv_app rec_elim_eqns in
      let if_then =
        S.mk_simple_app lemma_name (List.map ~f:(fun var -> S.mk_var var.vname) lemma_args)
      in
      [ S.mk_assert (S.mk_not (S.mk_forall quants (S.mk_or (S.mk_not if_condition) if_then))) ]

let verify_lemma_candidate ~(p : psi_def) (lemma_candidates : (symbol * variable list * term) list)
    negative_ctexs =
  Log.info (fun f () -> Fmt.(pf f "Checking lemma candidate..."));
  let _start_time = Unix.gettimeofday () in
  let solver = Smtlib.Solvers.make_cvc4_solver () in
  Solvers.set_logic solver "ALL";
  Solvers.set_option solver "quant-ind" "true";
  Solvers.set_option solver "produce-models" "true";
  if !Config.induction_proof_tlimit >= 0 then
    Solvers.set_option solver "tlimit" (Int.to_string !Config.induction_proof_tlimit);
  Solvers.load_min_max_defs solver;
  (* Declare Tinv, repr and reference functions. *)
  List.iter
    ~f:(fun x -> ignore (Solvers.exec_command solver x))
    ((match p.psi_tinv with None -> [] | Some tinv -> Smt.smt_of_pmrs tinv)
    @ (if p.psi_repr_is_identity then [] else Smt.smt_of_pmrs p.psi_repr)
    @ Smt.smt_of_pmrs p.psi_reference
    (* Declare lemmas. *)
    @ List.map
        ~f:(fun (name, vars, body) ->
          Smt.mk_def_fun_command name
            (List.map ~f:(fun v -> (v.vname, RType.TInt)) vars)
            RType.TBool body)
        lemma_candidates
    (* Check if lemmas are valid. *)
    @ List.concat_mapi
        ~f:(fun i lem -> smt_of_lemma_validity ~p lem (List.nth negative_ctexs i))
        (* TODO: associate lemma with term instead of ctex *)
        lemma_candidates);

  let _ =
    match Solvers.check_sat solver with
    (* TODO: If not unsat, lemma isn't valid. So, get counterexamples *)
    (* TODO: Check lemmas separately instead of all at once. *)
    (* | Sat -> (
        match Solvers.get_model solver with
        | SExps s ->
            let _model = Smt.model_to_constmap (SExps s) in
            ()
        | _ -> ()) *)
    | Unsat -> Log.info (fun f () -> Fmt.(pf f "This lemma is correct."))
    | Sat | Unknown ->
        Log.info (fun f () ->
            Fmt.(pf f "This lemma has not been proved correct. Refining lemma..."))
        (* TODO: refine lemma *)
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
  let maybe_lemmas, negative_ctex =
    match synt_failure_info with
    | _, Either.Second unrealizability_ctexs ->
        (* Forget about the specific association in pairs. *)
        let ctexs = List.concat_map unrealizability_ctexs ~f:(fun uc -> [ uc.ci; uc.cj ]) in
        (* Classify in negative and positive cexs. *)
        let _positive_ctexs, negative_ctexs = classify_ctexs_opt ~p ctexs in
        let set_logic = CSetLogic "DTLIA" in
        (* TODO: How to choose logic? *)
        let synth_objs = List.mapi ~f:synthfun_of_ctex negative_ctexs in
        (* TODO: Should synth lemma per term, not per ctex *)
        let constraints = List.mapi ~f:constraint_of_ctex negative_ctexs in
        let extra_defs = [ max_definition; min_definition ] in
        let commands = set_logic :: (extra_defs @ synth_objs @ constraints @ [ CCheckSynth ]) in
        ( handle_lemma_synth_response (Syguslib.Solvers.SygusSolver.solve_commands commands),
          negative_ctexs )
    | _ -> failwith "There is no synt_failure_info in synthesize_lemmas."
  in
  let _ =
    match maybe_lemmas with
    | None -> ()
    | Some lemma_candidates -> verify_lemma_candidate ~p lemma_candidates negative_ctex
  in
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
