open AState
open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils

let rec refinement_loop (p : psi_def) ((t_set, u_set) : TermSet.t * TermSet.t) =
  Int.incr refinement_steps;
  (* Output status information before entering process. *)
  let elapsed = Unix.gettimeofday () -. !Config.glob_start in
  Log.info (fun frmt () -> Fmt.pf frmt "Refinement step %i." !refinement_steps);
  (if not !Config.info then
   Fmt.(
     pf stdout "%i,%3.3f,%3.3f,%i,%i@." !refinement_steps !Config.verif_time elapsed
       (Set.length t_set) (Set.length u_set)));
  Log.debug_msg
    Fmt.(
      str "Start refinement loop with %i terms in T, %i terms in U." (Set.length t_set)
        (Set.length u_set));
  (* First, generate the set of constraints corresponding to the set of terms t_set. *)
  let eqns = Equations.make ~p t_set in
  (* The solve the set of constraints. *)
  let s_resp, solution = Equations.solve ~p eqns in
  match (s_resp, solution) with
  | RSuccess _, Some sol -> (
      (* Synthesis has succeeded, now we need to verify the solution. *)
      try
        (* The solution is verified with a bounded check.  *)
        let check_r = Verify.check_solution ~p (t_set, u_set) sol in
        match check_r with
        | Some (new_t_set, new_u_set) ->
            (* If check_r is some new set of MR-terms t_set, and terms u_set, this means
               verification failed. The generalized counterexamples have been added to new_t_set,
               which is also a superset of t_set.
            *)
            Log.debug (fun frmt () ->
                Fmt.(
                  pf frmt "@[<hov 2>Counterexample terms:@;@[<hov 2>%a@]" (list ~sep:comma pp_term)
                    (Set.elements (Set.diff new_t_set t_set))));
            (* Continue looping with the new sets. *)
            refinement_loop p (new_t_set, new_u_set)
        | None ->
            (* This case happens when verification succeeded. Return the solution. *)
            Log.print_ok ();
            Ok { soln_rec_scheme = p.target; soln_implems = sol }
      with _ -> (* A failure during the bounded check is an error. *)
                Error RFail)
  | _ -> (
      if
        !Config.interactive_lemmas_loop
        &&
        (Log.info (fun frmt () -> Fmt.pf frmt "No luck. Try again? (Y/N)");
         match Stdio.In_channel.input_line Stdio.stdin with
         | None | Some "" | Some "N" -> false
         | Some "Y" -> true
         | _ -> false)
      then refinement_loop p (t_set, u_set)
      else
        match (s_resp, solution) with
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
                    (list ~sep:sp pp_term) (Set.elements t_set));
            Error RInfeasible
        | RUnknown, _ ->
            (* In most cases if the synthesis solver does not find a solution and terminates, it will
               answer unknowns. We interpret it as "no solution can be found". *)
            Log.error_msg "SyGuS solver returned unknown.";
            Error RUnknown
        | _ -> Error s_resp)

let psi (p : psi_def) =
  (* Initialize sets with the most general terms. *)
  let t_set, u_set =
    if !Config.simple_init then
      let x0 = mk_var (Variable.mk ~t:(Some !AState._theta) (Alpha.fresh ())) in
      let s = TermSet.of_list (Analysis.expand_once x0) in
      Set.partition_tf ~f:(Expand.is_mr_all p) s
    else
      let init_set = MGT.most_general_terms p.target in
      Set.fold init_set ~init:(TermSet.empty, TermSet.empty) ~f:(fun (t, u) mgt ->
          let t', u' = Expand.to_maximally_reducible p mgt in
          (Set.union t t', Set.union u u'))
  in
  Log.debug (fun frmt () ->
      Fmt.(pf frmt "@[<hov 2>INIT = %a@]" (list ~sep:comma pp_term) (Set.elements t_set)));
  if Set.is_empty t_set then (
    Log.error_msg "Empty set of terms for equation system.";
    failwith "Cannot solve problem.")
  else (
    refinement_steps := 0;
    refinement_loop p (t_set, u_set))

(* ============================================================================================= *)
(*                                                 MAIN ENTRY POINTS                             *)
(* ============================================================================================= *)

let no_synth () = failwith "No synthesis objective found."

let solve_problem (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t) :
    (soln, solver_response) Result.t =
  let target_fname, spec_fname, repr_fname =
    match psi_comps with
    | Some names -> names
    | None ->
        Utils.Log.debug_msg "Using default names.";
        ("target", "spec", "repr")
  in
  (* Representation function. *)
  let repr, theta_to_tau =
    match Map.find pmrs repr_fname with
    | Some pmrs -> (Either.First pmrs, Variable.vtype_or_new pmrs.pmain_symb)
    | None -> (
        let reprs =
          Hashtbl.filter ~f:(fun (v, _, _, _) -> String.(v.vname = repr_fname)) Term._globals
        in
        match Hashtbl.choose reprs with
        | Some (_, (f, a, _, b)) -> (Either.Second (f, a, b), Variable.vtype_or_new f)
        (* No repr specified: assume identity. *)
        | None ->
            let x = Variable.mk "x" in
            let xt = Variable.vtype_or_new x in
            let repr_fun = Variable.mk ~t:(Some (TFun (xt, xt))) repr_fname in
            (Either.Second (repr_fun, [ PatVar x ], mk_var x), RType.TFun (xt, xt)))
  in
  (* Original function. *)
  let orig_f, tau =
    match Map.find pmrs spec_fname with
    | Some pmrs -> (
        match List.last pmrs.pinput_typ with
        | Some tau -> (pmrs, tau)
        | None ->
            Log.error_msg Fmt.(str "Reference function should have at least one input argument.");
            no_synth ())
    | None ->
        Log.error_msg Fmt.(str "No spec named %s found." spec_fname);
        no_synth ()
  in
  (* Target recursion scheme. *)
  let target_f, xi, theta =
    let target_f =
      match Map.find pmrs target_fname with
      | Some pmrs -> pmrs
      | None ->
          Log.error_msg Fmt.(str "No recursion skeleton named %s found." target_fname);
          no_synth ()
    in
    match List.last target_f.pinput_typ with
    | Some theta -> (target_f, target_f.psyntobjs, theta)
    | None ->
        Log.error_msg Fmt.(str "Recursion skeleton should have at least one input.");
        no_synth ()
  in
  (* Match origin and target recursion scheme types. *)
  (match RType.fun_typ_unpack theta_to_tau with
  | [ theta' ], tau' -> (
      let sb1 = RType.unify_one theta theta' in
      let sb2 = RType.unify_one tau tau' in
      match (sb1, sb2) with
      | Some sb1, Some sb2 -> (
          match RType.unify (RType.mkv (sb1 @ sb2)) with
          | Some sb' -> Term.Variable.update_var_types (RType.mkv sb')
          | None ->
              Log.error_msg "Could not unify θ and τ in problem definition.";
              Log.fatal ())
      | _ ->
          Log.error_msg
            (Fmt.str "repr has type %a, expected %a." RType.pp theta_to_tau RType.pp
               RType.(TFun (theta, tau)));
          Log.fatal ())
  | _ ->
      Log.error_msg "Representation function should be a function.";
      Log.fatal ());
  Term.(
    let orig_out = Variable.vtype_or_new orig_f.pmain_symb in
    let target_out = Variable.vtype_or_new target_f.pmain_symb in
    Log.debug_msg Fmt.(str "ɑ : unify %a and %a" RType.pp orig_out RType.pp target_out);
    match (orig_out, target_out) with
    | TFun (_, tout), TFun (_, tout') -> (
        match RType.unify_one tout tout' with
        | Some subs -> Variable.update_var_types (RType.mkv subs)
        | None ->
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
  let orig_f = PMRS.infer_pmrs_types orig_f in
  let args_t = target_f.pinput_typ in
  let t_out = orig_f.poutput_typ in
  (* Print summary information about the problem, before solving.*)
  Log.info
    Fmt.(
      fun fmt () ->
        pf fmt " Ψ (%a) := ∀ x : %a. (%s o %s)(x) = %s(x)"
          (list ~sep:comma Term.Variable.pp)
          (Set.elements xi) (list ~sep:sp RType.pp) args_t spec_fname repr_fname target_fname);
  Log.info Fmt.(fun fmt () -> pf fmt "%a" PMRS.pp orig_f);
  Log.info Fmt.(fun fmt () -> pf fmt "%a" PMRS.pp target_f);
  Log.info
    Fmt.(
      fun fmt () ->
        match repr with
        | Either.First pmrs -> pf fmt "%a" PMRS.pp pmrs
        | Either.Second (fv, args, body) ->
            pf fmt "%s(%a) = %a" fv.vname (list ~sep:comma Term.pp_fpattern) args Term.pp_term body);
  let repr_pmrs =
    match repr with Either.First p -> p | Either.Second (f, a, b) -> PMRS.func_to_pmrs f a b
  in
  if not (List.length orig_f.pargs = List.length target_f.pargs) then
    failwith "Specification and target recursion scheme must have the same number of parameters."
  else ();
  (* Set global information. *)
  AState._tau := tau;
  AState._theta := theta;
  AState._alpha := t_out;
  AState._span := List.length (Analysis.terms_of_max_depth 1 theta);
  AState.refinement_steps := 0;
  (* Solve the problem. *)
  let problem =
    {
      target = target_f;
      orig = orig_f;
      repr = repr_pmrs;
      repr_is_identity = Reduce.is_identity repr_pmrs;
    }
  in
  if !Config.use_acegis then Baselines.algo_acegis problem
  else if !Config.use_ccegis then Baselines.algo_ccegis problem
  else psi problem
