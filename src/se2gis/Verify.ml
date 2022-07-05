open Common
open ProblemDefs
open Base
open Lang
open Lang.SmtInterface
open Lang.Term
open Env
open Smtlib
open Utils

(* ============================================================================================= *)
(*                               Checking correctness of solutions                               *)
(* ============================================================================================= *)

let constr_eqn ~(ctx : env) (eqn : equation) =
  let rec mk_equality (lhs, rhs) =
    let lhs, rhs = ctx_reduce ctx lhs, ctx_reduce ctx rhs in
    match lhs.tkind, rhs.tkind with
    (* If terms are tuples, equate each tuple component and take conjunction. *)
    | TTup ltl, TTup rtl ->
      (match List.zip ltl rtl with
      | Ok rt_lt ->
        (match mk_assoc Binop.And (List.map ~f:mk_equality rt_lt) with
        | Some e -> e
        | None -> mk_const Constant.CTrue)
      | _ -> failwith "Verification failed: trying to equate tuples of different sizes?")
    (* Otherwise just make a smt term corresponding to the equation. *)
    | _ -> Terms.(lhs == rhs)
  in
  let equality = mk_equality (eqn.elhs, eqn.erhs) in
  ctx
  >-
  match eqn.eprecond with
  | Some inv ->
    let inv = Terms.typed ctx.ctx inv in
    smt_of_term Terms.((not inv) || equality)
  | None -> smt_of_term equality
;;

(* ============================================================================================= *)
(*                               Partially-bounded checking                                      *)
(* ============================================================================================= *)

let check_eqn solver has_sat eqn =
  let open AsyncSmt in
  let formula = SmtLib.mk_not eqn in
  let%lwt () = spush solver in
  let%lwt () = smt_assert solver formula in
  let%lwt x =
    match%lwt check_sat solver with
    | Sat -> Lwt.return (Stop true)
    | Success | Unsat -> Lwt.return (Continue has_sat)
    | Unknown -> failwith "Solver answered unknown during verification."
    | Error _ -> failwith "Solver encountered an error during verification."
    | Unsupported | SExps _ ->
      failwith
        "Solver returned unexpected answer during verification. Please inspect logs."
  in
  let%lwt () = spop solver in
  Lwt.return x
;;

(* During verification, some terms that have partially bounded might lack the lemmas that ensure
   verification is sound (the counterexamples obtained during verification might be spurious). In
   that case, the partially-bounded checking reverts to (fully-)bounded checking.
*)
let partial_bounding_checker
    ~(ctx : env)
    ~(p : PsiDef.t)
    (lstate : refinement_loop_state)
    (t_set : TermSet.t)
    : (term * term) list * refinement_loop_state
  =
  let f (acc_tset, acc_lstate) t =
    match ctx >- Specifications.get_requires p.PsiDef.target.pvar with
    | Some req ->
      (match Predicates.get ~count_reuse:false ~ctx ~p t with
      | Some _ ->
        (* Everything ok, a lemma has been computed *)
        acc_tset @ [ t, t ], acc_lstate
      | None ->
        (* There is a requires and no lemma, the term has to be bounded. *)
        let bt = ctx >- Expand.make_bounded t in
        let lem_t = ctx_reduce ctx (mk_app req [ bt ]) in
        let () = LemmasInteractive.set_term_lemma ~ctx ~p ~key:(bt, None) ~lemma:lem_t in
        acc_tset @ [ t, bt ], acc_lstate)
    | None -> acc_tset @ [ t, t ], acc_lstate
  in
  List.fold ~init:([], lstate) ~f (Set.elements t_set)
;;

let check_solution
    ~(ctx : env)
    ~(p : PsiDef.t)
    (lstate : refinement_loop_state)
    (soln : (string * variable list * term) list)
    : [ `Incorrect_assumptions
      | `witnesss of
        (term, Terms.comparator_witness) Set.t * (term, Terms.comparator_witness) Set.t
      | `Correct
      ]
    Lwt.t
  =
  let open AsyncSmt in
  Log.info (fun f () -> Fmt.(pf f "Checking solution..."));
  (* Turn of verbosity for bounded verification, otherwise too many messages appear. *)
  let verb = !Config.verbose in
  Config.verbose := false;
  let start_time = Unix.gettimeofday () in
  (* Replace the unknowns with their solution. *)
  let target_inst = ctx >>- Reduce.instantiate_with_solution p.PsiDef.target soln in
  let free_vars = VarSet.empty in
  let task (solver, binder) =
    let%lwt _ = binder in
    let preamble =
      let logic =
        ctx
        >- SmtLogic.infer_logic
             ~quantifier_free:true
             ~with_uninterpreted_functions:false
             ~logic_infos:[ p.PsiDef.reference.plogic; p.PsiDef.target.plogic ]
             []
      in
      Commands.mk_preamble
        ~incremental:(String.is_prefix ~prefix:"CVC" solver.s_name)
        ~logic
        ~models:true
        ()
      @ (ctx >- Commands.decls_of_vars free_vars)
    in
    (* Execute the preamble. *)
    let%lwt () = exec_all solver preamble in
    let expand_and_check i (t0 : term) =
      let t_set, u_set = Expand.to_maximally_reducible ~ctx p t0 in
      let t_set, _ = partial_bounding_checker ~ctx ~p lstate t_set in
      let num_terms_to_check = List.length t_set in
      if num_terms_to_check > 0
      then (
        let sys_eqns, _ =
          Equations.make
            ~count_reused_predicates:false
            ~ctx
            ~force_replace_off:true
            ~p:{ p with target = target_inst }
            ~lifting:lstate.lifting
            (TermSet.of_list (List.map ~f:snd t_set))
        in
        let smt_eqns = List.map sys_eqns ~f:(constr_eqn ~ctx) in
        (* Solver calls. *)
        let%lwt () = spush solver in
        (* Declare all the new variables used in the system of equations. *)
        let%lwt () =
          exec_all
            solver
            (ctx
            >- Commands.decls_of_vars
                 (Set.diff (Equations.free_vars_of_equations ~ctx sys_eqns) free_vars))
        in
        let%lwt has_witness =
          lwt_until ~init:(Lwt.return false) ~f:(check_eqn solver) smt_eqns
        in
        let%lwt () = spop solver in
        let select_unbound = if !Config.Optims.bound_after_verif then snd else fst in
        (* Result of solver calls. *)
        if has_witness
        then
          Lwt.return
            (true, TermSet.of_list (List.map ~f:select_unbound t_set), u_set, i + 1)
        else Lwt.return (false, TermSet.empty, u_set, i + num_terms_to_check))
      else (* set is empty *)
        Lwt.return (false, TermSet.empty, u_set, i)
    in
    let rec find_witness num_checks terms_to_expand =
      if num_checks > !Config.Optims.num_expansions_check
      then Lwt.return None (* Hit the unfolding limit. *)
      else (
        let next =
          List.filter
            ~f:(fun t -> term_height t <= !Config.Optims.check_depth)
            (Set.elements terms_to_expand)
        in
        match List.sort ~compare:term_height_compare next with
        | [] -> Lwt.return None
        | hd :: tl ->
          let%lwt has_witness, t_set, u_set, num_checks =
            expand_and_check num_checks hd
          in
          let elts = Set.union u_set (TermSet.of_list tl) in
          if has_witness
          then Lwt.return (Some (Set.union lstate.t_set t_set, elts))
          else find_witness num_checks elts)
    in
    (* Check that the solution is correct on current set T. If it is not, this is because of some wrong
  assumption made for optimization. *)
    match%lwt find_witness 0 lstate.t_set with
    | Some _ ->
      let%lwt () = close_solver solver in
      Lwt.return `Incorrect_assumptions
    | None ->
      let%lwt witness_or_none = find_witness 0 lstate.u_set in
      let%lwt () = close_solver solver in
      let elapsed = Unix.gettimeofday () -. start_time in
      Stats.add_verif_time elapsed;
      Log.info (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
      Config.verbose := verb;
      (match witness_or_none with
      | Some witness -> Lwt.return (`witnesss witness)
      | None -> Lwt.return `Correct)
  in
  let p, r = cancellable_task (make_solver !Config.verification_solver) task in
  Lwt.wakeup r 1;
  p
;;

(* ============================================================================================= *)
(*                               Bounded-checking                                                *)
(* ============================================================================================= *)

let bounded_check_eqn ?(use_concrete_witness = false) solver eqn =
  let open AsyncSmt in
  let formula = SmtLib.mk_not eqn in
  let%lwt () = spush solver in
  let%lwt () = smt_assert solver formula in
  let%lwt x = check_sat solver in
  let%lwt model =
    if use_concrete_witness
    then (
      match x with
      | Sat ->
        let%lwt m = get_model solver in
        Lwt.return (Some m)
      | _ -> Lwt.return None)
    else Lwt.return None
  in
  let%lwt () = spop solver in
  Lwt.return (x, model)
;;

(* Perform a bounded check of the solution *)
let bounded_check
    ?(use_concrete_witness = false)
    ~(ctx : env)
    ~(p : PsiDef.t)
    (soln : (string * variable list * term) list)
  =
  let open AsyncSmt in
  Log.info (fun f () -> Fmt.(pf f "Checking solution (bounded check)..."));
  let start_time = Unix.gettimeofday () in
  let target_inst = ctx >>- Lang.Reduce.instantiate_with_solution p.PsiDef.target soln in
  let free_vars = VarSet.empty in
  let init_vardecls = ctx >- Commands.decls_of_vars free_vars in
  let task (solver, binder) =
    let%lwt _ = binder in
    let preamble =
      let logic =
        ctx
        >- SmtLogic.infer_logic
             ~quantifier_free:true
             ~with_uninterpreted_functions:false
             ~logic_infos:[ p.PsiDef.reference.plogic; p.PsiDef.target.plogic ]
             []
      in
      Commands.mk_preamble
        ~incremental:(String.is_prefix ~prefix:"CVC" solver.s_name)
        ~logic
        ~models:true
        ()
      @ (ctx >- Commands.decls_of_vars free_vars)
    in
    (* Execute the preamble. *)
    let%lwt () = exec_all solver preamble in
    let%lwt () = load_min_max_defs solver in
    let tset =
      List.sort
        ~compare:term_size_compare
        (ctx
        >- Lang.Analysis.terms_of_max_depth
             (!Config.Optims.check_depth - 1)
             (get_theta ctx))
    in
    let check term =
      let sys_eqns, _ =
        Equations.make
          ~count_reused_predicates:false
          ~ctx
          ~force_replace_off:true
          ~p:{ p with target = target_inst }
          ~lifting:Lifting.empty_lifting
          (TermSet.singleton term)
      in
      let smt_eqns = List.map sys_eqns ~f:(fun t -> t, constr_eqn ~ctx t) in
      let new_free_vars =
        let f fv (eqn : equation) =
          Set.union
            fv
            (VarSet.union_list
               [ ctx >- Analysis.free_variables eqn.elhs
               ; ctx >- Analysis.free_variables eqn.erhs
               ; Option.(
                   value
                     ~default:VarSet.empty
                     (eqn.eprecond >>| fun t -> ctx >- Analysis.free_variables t))
               ])
        in
        Set.diff (List.fold ~f ~init:VarSet.empty sys_eqns) free_vars
      in
      let%lwt () = exec_all solver init_vardecls in
      let%lwt () = exec_all solver (ctx >- Commands.decls_of_vars new_free_vars) in
      let rec search_witness eqns =
        match eqns with
        | [] -> Lwt.return None
        | (eqn, smt_eqn) :: tl ->
          (match%lwt bounded_check_eqn ~use_concrete_witness solver smt_eqn with
          | Sat, Some model_response ->
            let model = ctx >>- model_to_constmap model_response in
            let concr = ctx >- Lang.Analysis.concretize ~model in
            let t, inv, lhs, rhs = eqn.eterm, eqn.eprecond, eqn.elhs, eqn.erhs in
            Lwt.return
              (Some
                 { eterm = concr t
                 ; eprecond = Option.map ~f:concr inv
                 ; esplitter = eqn.esplitter
                 ; elhs = concr lhs
                 ; erhs = concr rhs
                 ; eelim = []
                 })
          | Sat, None -> Lwt.return (Some eqn)
          | Error msg, _ ->
            Log.error_msg Fmt.(str "Solver failed with message: %s." msg);
            failwith "SMT Solver error."
          | SExps _, _ | Unsat, _ -> search_witness tl
          | Unknown, _ ->
            Log.error_msg
              Fmt.(str "SMT solver returned unknown. The solution might be incorrect.");
            search_witness tl
          | Unsupported, _ ->
            Log.error_msg
              Fmt.(str "SMT solver returned unsupported, which is unexpected.");
            search_witness tl
          | Success, _ ->
            (* Should not be a valid answer, but keep searching anyway. *)
            search_witness tl)
      in
      let%lwt witness_or_none = search_witness smt_eqns in
      let%lwt () =
        match witness_or_none with
        | Some eqn ->
          let witness_height = term_height eqn.eterm in
          let current_check_depth = !Config.Optims.check_depth in
          let update = max current_check_depth (witness_height - 1) in
          Config.Optims.check_depth := update;
          Log.verbose_msg Fmt.(str "Check depth: %i." !Config.Optims.check_depth);
          Lwt.return_unit
        | None -> Lwt.return_unit
      in
      Lwt.return witness_or_none
    in
    Log.debug_msg Fmt.(str "%i terms to check" (List.length tset));
    let%lwt witness_or_none =
      lwt_until tset ~init:(Lwt.return None) ~f:(fun _ t ->
          match%lwt check t with
          | Some witness -> Lwt.return (Stop (Some witness))
          | None -> Lwt.return (Continue None))
    in
    let%lwt () = close_solver solver in
    let elapsed = Unix.gettimeofday () -. start_time in
    Stats.add_verif_time elapsed;
    Log.info (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
    Lwt.return witness_or_none
  in
  let p, r =
    cancellable_task
      (make_solver ~hint:"bounded-checking " !Config.verification_solver)
      task
  in
  Lwt.wakeup r 1;
  p
;;

(* ============================================================================================= *)
(*                                 Other verification/checking functions                         *)
(* ============================================================================================= *)

let invert ~(ctx : env) (recf : PMRS.t) (c : Constant.t) : term list option Lwt.t =
  let open AsyncSmt in
  let check_bounded_sol solver terms =
    let f accum t =
      let vars = ctx >- Analysis.free_variables t in
      let f_t = ctx >>- Reduce.reduce_pmrs recf t in
      let f_t_eq_c = mk_bin Eq f_t (mk_const c) in
      let%lwt () = spush solver in
      let%lwt () = exec_all solver (ctx >- Commands.decls_of_vars vars) in
      let%lwt () = smt_assert solver (ctx >- smt_of_term f_t_eq_c) in
      let sol =
        match%lwt check_sat solver with
        | Sat ->
          let%lwt m = get_model solver in
          let model_as_subst = VarMap.to_subst ctx.ctx (ctx >>- model_to_varmap vars m) in
          Lwt.return (Stop (Some (ctx_reduce ctx (substitution model_as_subst t))))
        | _ -> Lwt.return (Continue accum)
      in
      let%lwt () = spop solver in
      sol
    in
    lwt_until ~init:(Lwt.return None) ~f (Set.elements terms)
  in
  match recf.pinput_typ with
  | [ typ1 ] ->
    let x1 = Variable.mk ctx.ctx ~t:(Some typ1) (Alpha.fresh ctx.ctx.names) in
    let task (solver, binder) =
      let%lwt _ = binder in
      let rec expand_loop u =
        match Set.min_elt u with
        | Some t0 ->
          let tset, u' = ctx >- Expand.simple t0 in
          (match%lwt check_bounded_sol solver tset with
          | Some s -> Lwt.return_some s
          | None -> expand_loop (Set.union (Set.remove u t0) u'))
        | None -> Lwt.return_none
      in
      let res = expand_loop (TermSet.singleton (mk_var ctx.ctx x1)) in
      let%lwt () = close_solver solver in
      res
    in
    let p, r = cancellable_task (make_solver !Config.verification_solver) task in
    Lwt.wakeup r 1;
    Lwt.map (fun res -> Option.map ~f:(fun x -> [ x ]) res) p
  | _ ->
    Log.error_msg "rec_inverse: only PMRS with a single input argument supported for now.";
    Lwt.return None
;;
