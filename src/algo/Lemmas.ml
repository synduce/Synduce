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
module T = Term

let make_term_state_detail (term : term) =
  {
    term;
    lemmas = [];
    lemma_candidate = None;
    negative_ctexs = [];
    positive_ctexs = [];
    recurs_elim = [];
    vars = Analysis.free_variables term;
    ctex =
      (* TODO: this is hacky. Make ctex optional here. *)
      {
        ctex_vars = VarSet.empty;
        ctex_eqn = { eterm = term; eprecond = None; eelim = []; elhs = term; erhs = term };
        ctex_model = Map.empty (module Int);
      };
  }

let make_term_state_detail_from_ctex (is_pos_ctex : bool) (ctex : ctex) : term_state_detail =
  let neg = if is_pos_ctex then [] else [ ctex ] in
  let pos = if is_pos_ctex then [ ctex ] else [] in
  {
    term = ctex.ctex_eqn.eterm;
    lemmas = [];
    lemma_candidate = None;
    negative_ctexs = neg;
    positive_ctexs = pos;
    recurs_elim = ctex.ctex_eqn.eelim;
    vars = Set.union (Analysis.free_variables ctex.ctex_eqn.eterm) ctex.ctex_vars;
    ctex;
  }

let term_detail_to_lemma (det : term_state_detail) : term option = T.mk_assoc Binop.And det.lemmas

let empty_term_state : term_state = Map.empty (module Terms)

let set_term_lemma (ts : term_state) ~(key : term) ~(lemma : term) : term_state =
  match Map.find ts key with
  | None -> Map.add_exn ts ~key ~data:(make_term_state_detail key)
  | Some det -> Map.add_exn ts ~key ~data:{ det with lemmas = [ lemma ] }

let add_ctexs_of_term (ts : term_state) ~(key : term) ~(pos_ctexs : ctex list)
    ~(neg_ctexs : ctex list) : term_state =
  Map.update ts key ~f:(fun lem ->
      match lem with
      | None -> failwith "set_lemma failure: could not find lemma detail for this term."
      | Some lem ->
          {
            lem with
            positive_ctexs = lem.positive_ctexs @ pos_ctexs;
            negative_ctexs = lem.negative_ctexs @ neg_ctexs;
          })

let create_or_update_term_state_for_ctex (is_pos_ctex : bool) (ts : term_state) (ctex : ctex) :
    term_state =
  match Map.find ts ctex.ctex_eqn.eterm with
  | None ->
      Map.add_exn ~key:ctex.ctex_eqn.eterm
        ~data:(make_term_state_detail_from_ctex is_pos_ctex ctex)
        ts
  | Some _ ->
      Map.update ts ctex.ctex_eqn.eterm ~f:(fun maybe_det ->
          match maybe_det with
          | None -> failwith "Term detail does not exist."
          | Some det ->
              if is_pos_ctex then { det with positive_ctexs = ctex :: det.positive_ctexs }
              else { det with negative_ctexs = ctex :: det.negative_ctexs })

let update_term_state_for_ctexs (ts : term_state) ~(pos_ctexs : ctex list) ~(neg_ctexs : ctex list)
    : term_state =
  List.fold
    ~init:(List.fold ~init:ts ~f:(create_or_update_term_state_for_ctex true) pos_ctexs)
    ~f:(create_or_update_term_state_for_ctex false)
    neg_ctexs

let get_lemma (ts : term_state) ~(key : term) : term option =
  match Map.find ts key with None -> None | Some det -> term_detail_to_lemma det

let get_term_state_detail (ts : term_state) ~(key : term) = Map.find ts key

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
        | Some x -> set_term_lemma existing_lemmas ~key:t ~lemma:(term x))
  in
  { lstate with term_state = Set.fold ~f ~init:lstate.term_state lstate.t_set }

let ith_synth_fun index = "lemma_" ^ Int.to_string index

let synthfun_of_ctex (det : term_state_detail) (lem_id : int) : command =
  let args =
    List.map
      ~f:(fun scalar -> (scalar.vname, sort_of_rtype RType.TInt))
      (Set.elements det.ctex.ctex_vars)
  in
  let ret_sort = sort_of_rtype RType.TBool in
  let grammar = Grammars.generate_grammar ~guess:None ~bools:true OpSet.empty args ret_sort in
  CSynthFun (ith_synth_fun lem_id, args, ret_sort, grammar)

let constraint_of_neg_ctex index ctex =
  let var_list = List.map ~f:(fun t -> sygus_of_term t) (Map.data ctex.ctex_model) in
  (* TODO: check that order of terms in Map.data model is the same as order of arguments to xi *)
  CConstraint (SyApp (IdSimple "not", [ SyApp (IdSimple (ith_synth_fun index), var_list) ]))

let constraint_of_pos_ctex index ctex =
  let var_list = List.map ~f:(fun t -> sygus_of_term t) (Map.data ctex.ctex_model) in
  (* TODO: check that order of terms in Map.data model is the same as order of arguments to xi *)
  CConstraint (SyApp (IdSimple (ith_synth_fun index), var_list))

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

let smt_of_recurs_elim_eqns (elim : (term * term) list) ~(p : psi_def) : S.smtTerm =
  let f_compose_r t =
    let repr_of_v = if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pvar [ t ] in
    mk_app_v p.psi_reference.pvar (List.map ~f:mk_var p.psi_reference.pargs @ [ repr_of_v ])
  in
  S.mk_assoc_and
    (List.map
       ~f:(fun (t1, t2) -> S.mk_eq (Smt.smt_of_term (f_compose_r t1)) (Smt.smt_of_term t2))
       elim)

let smt_of_tinv_app ~(p : psi_def) (det : term_state_detail) =
  match p.psi_tinv with
  | None -> failwith "No TInv has been specified. Cannot make smt of tinv app."
  | Some pmrs -> S.mk_simple_app pmrs.pvar.vname [ Smt.smt_of_term det.term ]

let smt_of_lemma_app (lemma_name, lemma_args, _lemma_body) =
  S.mk_simple_app lemma_name (List.map ~f:(fun var -> S.mk_var var.vname) lemma_args)

let smt_of_lemma_validity ~(p : psi_def) lemma (det : term_state_detail) =
  let mk_sort maybe_rtype =
    match maybe_rtype with None -> S.mk_int_sort | Some rtype -> Smt.sort_of_rtype rtype
  in
  let quants =
    List.map
      ~f:(fun var -> (S.SSimple var.vname, mk_sort (Variable.vtype var)))
      (Set.elements det.vars @ p.psi_reference.pargs)
  in
  (* TODO: if condition should account for other preconditions *)
  let if_condition =
    S.mk_and (smt_of_tinv_app ~p det) (smt_of_recurs_elim_eqns det.recurs_elim ~p)
  in
  let if_then = smt_of_lemma_app lemma in
  [ S.mk_assert (S.mk_not (S.mk_forall quants (S.mk_or (S.mk_not if_condition) if_then))) ]

let verify_lemma_candidate ~(p : psi_def) (det : term_state_detail) :
    Solvers.online_solver * Solvers.solver_response =
  match det.lemma_candidate with
  | None -> failwith "Cannot verify lemma candidate; there is none."
  | Some lemma_candidate ->
      Log.info (fun f () -> Fmt.(pf f "Checking lemma candidate..."));
      let _start_time = Unix.gettimeofday () in
      (* This solver is later closed in lemma_refinement_loop *)
      (* TODO: don't redo all the setup work in every iteration of the lemma refinement loop. Use push/pop instead. *)
      let solver = Smtlib.Solvers.make_cvc4_solver () in
      Solvers.set_logic solver "ALL";
      Solvers.set_option solver "quant-ind" "true";
      Solvers.set_option solver "produce-models" "true";
      Solvers.set_option solver "incremental" "true";
      if !Config.induction_proof_tlimit >= 0 then
        Solvers.set_option solver "tlimit" (Int.to_string !Config.induction_proof_tlimit);
      Solvers.load_min_max_defs solver;
      (* Declare Tinv, repr and reference functions. *)
      List.iter
        ~f:(fun x -> ignore (Solvers.exec_command solver x))
        ((match p.psi_tinv with None -> [] | Some tinv -> Smt.smt_of_pmrs tinv)
        @ (if p.psi_repr_is_identity then Smt.smt_of_pmrs p.psi_reference
          else Smt.smt_of_pmrs p.psi_repr @ Smt.smt_of_pmrs p.psi_reference)
        (* Declare lemmas. *)
        @ [
            (match lemma_candidate with
            | name, vars, body ->
                Smt.mk_def_fun_command name
                  (List.map ~f:(fun v -> (v.vname, RType.TInt)) vars)
                  RType.TBool body);
          ]
        @ smt_of_lemma_validity ~p lemma_candidate det);

      let result = Solvers.check_sat solver in
      (solver, result)

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

let get_positive_examples (solver : Solvers.online_solver) (det : term_state_detail) ~(p : psi_def)
    lemma : ctex list =
  (* TODO: make fresh variables, since this may in the future be called for multiple terms, for multiple lemma candidates, or to generate multiple pos examples, in one solver instance *)
  Log.info (fun f () -> Fmt.(pf f "Searching for a positive example."));
  let vset = det.vars in
  let _ = Solvers.declare_all solver (Smt.decls_of_vars vset) in
  let rec_elim_eqns = smt_of_recurs_elim_eqns det.recurs_elim ~p in
  let _ =
    List.iter
      ~f:(fun command -> ignore (Solvers.exec_command solver command))
      [
        S.mk_assert rec_elim_eqns;
        S.mk_assert (smt_of_tinv_app ~p det);
        S.mk_assert (S.mk_not (smt_of_lemma_app lemma));
      ]
  in
  match Solvers.check_sat solver with
  | Sat | Unknown -> (
      match Solvers.get_model solver with
      | SExps s ->
          let model = Smt.model_to_constmap (SExps s) in
          let m, _ =
            Map.partitioni_tf
              ~f:(fun ~key ~data:_ -> Option.is_some (VarSet.find_by_name det.ctex.ctex_vars key))
              model
          in
          (* Remap the names to ids of the original variables in m' *)
          [
            ({
               det.ctex with
               ctex_model =
                 Map.fold
                   ~init:(Map.empty (module Int))
                   ~f:(fun ~key ~data acc ->
                     match VarSet.find_by_name det.ctex.ctex_vars key with
                     | None -> acc
                     | Some var -> Map.set ~data acc ~key:var.vid)
                   m;
             }
              : ctex);
          ]
      | _ -> failwith "Get model failure: Positive example cannot be found during lemma refinement."
      )
  | _ -> failwith "Check sat failure: Positive example cannot be found during lemma refinement."

let synthesize_new_lemma (det : term_state_detail) : (string * variable list * term) option =
  let set_logic = CSetLogic "DTLIA" in
  (* TODO: How to choose logic? *)
  let lem_id = 0 in
  let synth_objs = synthfun_of_ctex det lem_id in
  let neg_constraints = List.map ~f:(constraint_of_neg_ctex lem_id) det.negative_ctexs in
  let pos_constraints = List.map ~f:(constraint_of_pos_ctex lem_id) det.positive_ctexs in
  let extra_defs = [ max_definition; min_definition ] in
  let commands =
    set_logic :: (extra_defs @ [ synth_objs ] @ neg_constraints @ pos_constraints @ [ CCheckSynth ])
  in
  match handle_lemma_synth_response (Syguslib.Solvers.SygusSolver.solve_commands commands) with
  | None -> None
  | Some solns -> List.nth solns 0
(* TODO: will the lemma we wanted to synth always be first? *)

let rec lemma_refinement_loop (det : term_state_detail) ~(p : psi_def) : term_state_detail =
  match synthesize_new_lemma det with
  | None -> failwith "Failure synthesizing new lemma."
  | Some (name, vars, lemma_term) -> (
      match
        verify_lemma_candidate ~p { det with lemma_candidate = Some (name, vars, lemma_term) }
      with
      | _, Unsat ->
          Log.info (fun f () -> Fmt.(pf f "This lemma is correct."));
          { det with lemma_candidate = None; lemmas = lemma_term :: det.lemmas }
      | solver, Sat | solver, Unknown ->
          Log.info (fun f () ->
              Fmt.(pf f "This lemma has not been proved correct. Refining lemma..."));
          let new_positive_ctexs = get_positive_examples solver det ~p (name, vars, lemma_term) in
          List.iter
            ~f:(fun ctex ->
              Log.info (fun f () -> Fmt.(pf f "Found a positive example: %a" (box pp_ctex) ctex)))
            new_positive_ctexs;
          Solvers.close_solver solver;
          lemma_refinement_loop
            { det with positive_ctexs = det.positive_ctexs @ new_positive_ctexs }
            ~p
      | _ -> failwith "Lemma verification returned something other than Unsat, Sat, or Unknown.")

let synthesize_lemmas ~(p : psi_def) synt_failure_info (lstate : refinement_loop_state) :
    (refinement_loop_state, solver_response) Result.t =
  (*
    Example: the synt_failure_info should be a list of unrealizability counterexamples, which
    are pairs of counterexamples.
    Each counterexample can be classfied as positive or negative w.r.t to the predicate p.psi_tinv.
    The lemma corresponding to a particular term should be refined to eliminate the counterexample
    (a counterexample cex is also asscociated to a particular term through cex.ctex_eqn.eterm)
   *)
  let new_state =
    match synt_failure_info with
    | _, Either.Second unrealizability_ctexs ->
        (* Forget about the specific association in pairs. *)
        let ctexs = List.concat_map unrealizability_ctexs ~f:(fun uc -> [ uc.ci; uc.cj ]) in
        (* Classify in negative and positive cexs. *)
        let _, negative_ctexs = classify_ctexs_opt ~p ctexs in
        let ts : term_state =
          update_term_state_for_ctexs lstate.term_state ~neg_ctexs:negative_ctexs ~pos_ctexs:[]
        in
        if List.is_empty negative_ctexs then ts
        else Map.map ts ~f:(fun det -> lemma_refinement_loop det ~p)
    | _ -> failwith "There is no synt_failure_info in synthesize_lemmas."
  in
  if
    !Config.interactive_lemmas_loop
    &&
    (Log.info (fun frmt () -> Fmt.pf frmt "No luck. Try again? (Y/N)");
     match Stdio.In_channel.input_line Stdio.stdin with
     | None | Some "" | Some "N" -> false
     | Some "Y" -> true
     | _ -> false)
  then Ok { lstate with term_state = new_state }
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
