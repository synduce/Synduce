open AState
open Base
open Lang
open Lang.SmtInterface
open Lang.Term
open Smtlib
open Utils

(* ============================================================================================= *)
(*                               Checking correctness of solutions                               *)
(* ============================================================================================= *)

let constr_eqn (eqn : equation) =
  let rec mk_equality (lhs, rhs) =
    let lhs, rhs = (Lang.Reduce.reduce_term lhs, Lang.Reduce.reduce_term rhs) in
    match (lhs.tkind, rhs.tkind) with
    (* If terms are tuples, equate each tuple component and take conjunction. *)
    | TTup ltl, TTup rtl -> (
        match List.zip ltl rtl with
        | Ok rt_lt -> SmtLib.mk_assoc_and (List.map ~f:mk_equality rt_lt)
        | _ -> failwith "Verification failed: trying to equate tuples of different sizes?")
    (* Otherwise just make a smt terms corresponding to the equation. *)
    | _ -> SmtLib.mk_eq (smt_of_term lhs) (smt_of_term rhs)
  in
  let equality = mk_equality (eqn.elhs, eqn.erhs) in
  match eqn.eprecond with
  | Some inv -> SmtLib.mk_or (SmtLib.mk_not (smt_of_term inv)) equality
  | None -> equality

(* ============================================================================================= *)
(*                               Partially-bounded checking                                      *)
(* ============================================================================================= *)

let check_eqn solver has_sat eqn =
  let formula = SmtLib.mk_not eqn in
  SyncSmt.spush solver;
  SyncSmt.smt_assert solver formula;
  let x =
    match SyncSmt.check_sat solver with
    | Sat -> Continue_or_stop.Stop true
    | _ -> Continue_or_stop.Continue has_sat
  in
  SyncSmt.spop solver;
  x

(* During verification, some terms that have partially bounded might lack the lemmas that ensure
   verification is sound (the counterexamples obtained during verification might be spurious). In
   that case, the partially-bounded checking reverts to (fully-)bounded checking.
*)
let partial_bounding_checker ~(p : psi_def) (lstate : refinement_loop_state) (t_set : TermSet.t) :
    TermSet.t * refinement_loop_state =
  let f (acc_tset, acc_lstate) t =
    match Specifications.get_requires p.psi_target.pvar with
    | Some req -> (
        match Lemmas.get_lemma ~p acc_lstate.term_state ~key:t with
        | Some _ ->
            (* Everything ok, a lemma has been computed *)
            (Set.add acc_tset t, acc_lstate)
        | None ->
            (* There is a requires and no lemma, the term has to be bounded. *)
            let bt = Expand.make_bounded t in
            let lem_t = Reduce.reduce_term (mk_app req [ bt ]) in
            let lem_info = Lemmas.set_term_lemma ~p acc_lstate.term_state ~key:bt ~lemma:lem_t in
            (Set.add acc_tset bt, { acc_lstate with term_state = lem_info }))
    | None -> (Set.add acc_tset t, acc_lstate)
  in
  List.fold ~init:(TermSet.empty, lstate) ~f (Set.elements t_set)

let check_solution ?(use_acegis = false) ~(p : psi_def) (lstate : refinement_loop_state)
    (soln : (string * variable list * term) list) =
  if use_acegis then Log.info (fun f () -> Fmt.(pf f "Check solution."))
  else Log.info (fun f () -> Fmt.(pf f "Checking solution..."));
  let verb = !Config.verbose in
  Config.verbose := false;
  let start_time = Unix.gettimeofday () in
  let target_inst = Lang.Reduce.instantiate_with_solution p.psi_target soln in
  let free_vars = VarSet.empty in
  let init_vardecls = decls_of_vars free_vars in
  let solver = SyncSmt.make_z3_solver () in
  SyncSmt.load_min_max_defs solver;
  let expand_and_check i (t0 : term) =
    let t_set, u_set = Expand.to_maximally_reducible p t0 in
    let t_set, tmp_lstate = partial_bounding_checker ~p lstate t_set in
    let num_terms_to_check = Set.length t_set in
    if num_terms_to_check > 0 then (
      let sys_eqns, _ =
        Equations.make ~force_replace_off:true
          ~p:{ p with psi_target = target_inst }
          ~term_state:tmp_lstate.term_state ~lifting:lstate.lifting t_set
      in
      let smt_eqns = List.map sys_eqns ~f:constr_eqn in
      (* Solver calls. *)
      SyncSmt.spush solver;
      (* Declare all the new variables used in the system of equations. *)
      SyncSmt.declare_all solver
        (decls_of_vars (Set.diff (Equations.free_vars_of_equations sys_eqns) free_vars));
      let has_ctex =
        List.fold_until ~finish:(fun x -> x) ~init:false ~f:(check_eqn solver) smt_eqns
      in
      SyncSmt.spop solver;
      (* Result of solver calls. *)
      if has_ctex then (true, t_set, u_set, i + 1)
      else (false, TermSet.empty, u_set, i + num_terms_to_check))
    else (
      (* set is empty *)
      Log.debug_msg Fmt.(str "Checked all terms.");
      (false, TermSet.empty, u_set, i))
  in
  let rec find_ctex num_checks terms_to_expand =
    Log.verbose_msg Fmt.(str "Check %i." num_checks);
    if num_checks > !Config.num_expansions_check then (
      Log.debug_msg Fmt.(str "Hit unfolding limit.");
      None)
    else
      let next =
        List.filter
          ~f:(fun t -> term_height t <= !Config.check_depth + 1)
          (Set.elements terms_to_expand)
      in
      match List.sort ~compare:term_height_compare next with
      | [] -> None
      | hd :: tl ->
          let has_ctex, t_set, u_set, num_checks = expand_and_check num_checks hd in
          if has_ctex then Some (Set.union lstate.t_set t_set, terms_to_expand)
          else
            let elts = Set.union u_set (TermSet.of_list tl) in
            find_ctex num_checks elts
  in
  (* Declare all variables *)
  SyncSmt.declare_all solver init_vardecls;
  (match find_ctex 0 lstate.t_set with
  | Some _ -> failwith "Synthesizer and solver disagree on solution. That's unexpected!"
  | None -> ());
  let ctex_or_none = find_ctex 0 lstate.u_set in
  SyncSmt.close_solver solver;
  let elapsed = Unix.gettimeofday () -. start_time in
  Stats.add_verif_time elapsed;
  Log.info (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
  Config.verbose := verb;
  ctex_or_none

(* ============================================================================================= *)
(*                               Bounded-checking                                                *)
(* ============================================================================================= *)

let bounded_check_eqn ?(use_concrete_ctex = false) solver eqn =
  let formula = SmtLib.mk_not eqn in
  SyncSmt.spush solver;
  SyncSmt.smt_assert solver formula;
  let x = SyncSmt.check_sat solver in
  let model =
    if use_concrete_ctex then match x with Sat -> Some (SyncSmt.get_model solver) | _ -> None
    else None
  in
  SyncSmt.spop solver;
  (x, model)

(* Perform a bounded check of the solution *)
let bounded_check ?(use_concrete_ctex = false) ~(p : psi_def)
    (soln : (string * variable list * term) list) =
  Log.info (fun f () -> Fmt.(pf f "Checking solution (bounded check)..."));
  let start_time = Unix.gettimeofday () in
  let target_inst = Lang.Reduce.instantiate_with_solution p.psi_target soln in
  let free_vars = VarSet.empty in
  let init_vardecls = decls_of_vars free_vars in
  let solver = SyncSmt.make_z3_solver () in
  SyncSmt.load_min_max_defs solver;
  let check term =
    let sys_eqns, _ =
      Equations.make ~force_replace_off:true
        ~p:{ p with psi_target = target_inst }
        ~term_state:Lemmas.empty_term_state ~lifting:Lifting.empty_lifting (TermSet.singleton term)
    in
    let smt_eqns = List.map sys_eqns ~f:(fun t -> (t, constr_eqn t)) in
    let new_free_vars =
      let f fv (eqn : equation) =
        Set.union fv
          (Set.union
             (Lang.Analysis.free_variables eqn.elhs)
             (Lang.Analysis.free_variables eqn.erhs))
      in
      Set.diff (List.fold ~f ~init:VarSet.empty sys_eqns) free_vars
    in
    SyncSmt.declare_all solver init_vardecls;
    SyncSmt.declare_all solver (decls_of_vars new_free_vars);
    let rec search_ctex _eqns =
      match _eqns with
      | [] -> None
      | (eqn, smt_eqn) :: tl -> (
          match bounded_check_eqn ~use_concrete_ctex solver smt_eqn with
          | Sat, Some model_response ->
              let model = model_to_constmap model_response in
              let concr = Lang.Analysis.concretize ~model in
              let t, inv, lhs, rhs = (eqn.eterm, eqn.eprecond, eqn.elhs, eqn.erhs) in
              Some
                {
                  eterm = concr t;
                  eprecond = Option.map ~f:concr inv;
                  elhs = concr lhs;
                  erhs = concr rhs;
                  eelim = [];
                }
          | Sat, None -> Some eqn
          | Error msg, _ ->
              Log.error_msg Fmt.(str "Solver failed with message: %s." msg);
              failwith "SMT Solver error."
          | SExps _, _ | Unsat, _ -> search_ctex tl
          | Unknown, _ ->
              Log.error_msg Fmt.(str "SMT solver returned unkown. The solution might be incorrect.");
              search_ctex tl
          | Unsupported, _ ->
              Log.error_msg Fmt.(str "SMT solver returned unsupported, which is unexpected.");
              search_ctex tl
          | Success, _ ->
              (* Should not be a valid answer, but keep searching anyway. *) search_ctex tl)
    in
    let ctex_or_none = search_ctex smt_eqns in
    (match ctex_or_none with
    | Some eqn ->
        let ctex_height = term_height eqn.eterm in
        let current_check_depth = !Config.check_depth in
        let update = max current_check_depth (ctex_height - 1) in
        Config.check_depth := update;
        Log.verbose_msg Fmt.(str "Check depth: %i." !Config.check_depth)
    | None -> ());
    ctex_or_none
  in
  let tset =
    List.sort ~compare:term_size_compare
      (Lang.Analysis.terms_of_max_depth (!Config.check_depth - 1) !AState._theta)
  in
  Log.debug_msg Fmt.(str "%i terms to check" (List.length tset));
  let ctex_or_none =
    List.fold_until tset ~init:None
      ~finish:(fun eqn -> eqn)
      ~f:(fun _ t ->
        match check t with
        | Some ctex -> Continue_or_stop.Stop (Some ctex)
        | None -> Continue_or_stop.Continue None)
  in
  SyncSmt.close_solver solver;
  let elapsed = Unix.gettimeofday () -. start_time in
  Stats.add_verif_time elapsed;
  Log.info (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
  ctex_or_none

(* ============================================================================================= *)
(*                                 Other verification/checking functions                         *)
(* ============================================================================================= *)

let invert (recf : PMRS.t) (c : Constant.t) : term list option =
  let check_bounded_sol solver terms =
    let f accum t =
      let vars = Analysis.free_variables t in
      let f_t = Reduce.reduce_pmrs recf t in
      let f_t_eq_c = mk_bin Eq f_t (mk_const c) in
      SyncSmt.spush solver;
      SyncSmt.declare_all solver (SmtInterface.decls_of_vars vars);
      SyncSmt.smt_assert solver (smt_of_term f_t_eq_c);
      let sol =
        match SyncSmt.check_sat solver with
        | Sat ->
            let model_as_subst =
              VarMap.to_subst (model_to_varmap vars (SyncSmt.get_model solver))
            in
            Continue_or_stop.Stop (Some (Reduce.reduce_term (substitution model_as_subst t)))
        | _ -> Continue_or_stop.Continue accum
      in
      SyncSmt.spop solver;
      sol
    in
    Set.fold_until ~init:None ~finish:(fun x -> x) ~f terms
  in
  match recf.pinput_typ with
  | [ typ1 ] ->
      let x1 = Variable.mk ~t:(Some typ1) (Alpha.fresh ()) in
      let solver = SyncSmt.make_z3_solver () in
      let rec expand_loop u =
        match Set.min_elt u with
        | Some t0 -> (
            let tset, u' = Expand.simple t0 in
            match check_bounded_sol solver tset with
            | Some s -> Some s
            | None -> expand_loop (Set.union (Set.remove u t0) u'))
        | None -> None
      in
      let res = expand_loop (TermSet.singleton (mk_var x1)) in
      SyncSmt.close_solver solver;
      Option.map ~f:(fun x -> [ x ]) res
  | _ ->
      Log.error_msg "rec_inverse: only PMRS with a single input argument supported for now.";
      None
