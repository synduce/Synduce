open AState
open Base
open Lang
open Lang.Term
open Projection
open Syguslib.Sygus
open SygusInterface
open Utils
open Lwt.Syntax

(* ============================================================================================= *)
(*                               BUILDING SYSTEMS OF EQUATIONS                                   *)
(* ============================================================================================= *)

let check_equation ~(p : psi_def) (eqn : equation) : bool =
  (match (Expand.nonreduced_terms_all p eqn.elhs, Expand.nonreduced_terms_all p eqn.erhs) with
  | [], [] -> true
  | _ -> false)
  &&
  match eqn.eprecond with
  | None -> true
  | Some t -> ( match Expand.nonreduced_terms_all p t with [] -> true | _ -> false)

(**
   Compute the left hand side of an equation of p from term t.
   The result is a maximally reduced term with some applicative
   terms of the form (p.psi_reference x) where x is a variable.
*)
let compute_lhs p t =
  let t' = Reduce.reduce_pmrs p.psi_repr t in
  let r_t = Expand.replace_rhs_of_main p p.psi_repr t' in
  let subst_params =
    let l = List.zip_exn p.psi_reference.pargs p.psi_target.pargs in
    List.map l ~f:(fun (v1, v2) -> (mk_var v1, mk_var v2))
  in
  let f_r_t = Reduce.reduce_pmrs p.psi_reference r_t in
  let final = substitution subst_params f_r_t in
  Expand.replace_rhs_of_mains p (Reduce.reduce_term final)

let remap_rec_calls p t =
  let proj_func = Lifting.proj_to_lifting p in
  let lift_func = Lifting.compose_parts p in
  let g = p.psi_target in
  let lift_wrapper tx =
    match (proj_func, lift_func) with
    | Some pf, Some lf ->
        let t1 = mk_app (mk_var g.pmain_symb) [ tx ] in
        let t2 = mk_box (pf t1) in
        mk_app lf [ compute_lhs p tx; t2 ]
    | _ -> compute_lhs p tx
  in
  let t' = Expand.replace_rhs_of_main p g t in
  let f a t0 =
    match t0.tkind with
    | TApp ({ tkind = TVar g'; _ }, args) ->
        if a && Variable.equal g' g.pmain_symb then
          match args with
          | [ { tkind = TVar _; _ } ] -> Either.Second a
          | [ arg ] -> Either.First (lift_wrapper arg)
          | _ -> Either.Second a
        else if Set.mem g.psyntobjs g' then Second true
        else Second a
    | _ -> Second a
  in
  let res = rewrite_accum ~init:false ~f t' in
  if Term.term_equal res t' then t (* Don't revert step taken before *) else res

let compute_rhs_with_replacing p t =
  let g = p.psi_target in
  let custom_reduce x =
    let one_step t0 =
      let rstep = ref false in
      let rewrite_rule _t =
        match _t.tkind with
        | TApp ({ tkind = TVar f; _ }, fargs) -> (
            match Reduce.rule_lookup g.prules f fargs with
            | [] -> None
            | hd :: _ ->
                let hd' = remap_rec_calls p hd in
                rstep := true;
                Some hd')
        (* Replace recursive calls to g by calls to f circ g,
           if recursive call appear under unknown. *)
        | _ -> None
      in
      let t0' = rewrite_top_down rewrite_rule t0 in
      (t0', !rstep)
    in
    Reduce.until_irreducible one_step x
  in
  let app_t = mk_app (mk_var g.pmain_symb) [ t ] in
  let t' = Reduce.reduce_term (custom_reduce app_t) in
  let _res = Expand.replace_rhs_of_mains p t' in
  _res

let compute_rhs ?(force_replace_off = false) p t =
  if not force_replace_off then
    let res = compute_rhs_with_replacing p t in
    res
  else
    let res =
      Expand.replace_rhs_of_mains p (Reduce.reduce_term (Reduce.reduce_pmrs p.psi_target t))
    in
    res

let compute_preconds ~p ~term_state subst eterm =
  match Lemmas.get_lemma ~p term_state ~key:eterm with
  | Some lemma_for_eterm ->
      let t = Reduce.reduce_term (subst lemma_for_eterm) in
      Some t
  | None ->
      (* If the term is bounded and there is a invariant, add a precondition.
         This will avoid calls to the lemma synthesis.
      *)
      if Analysis.is_bounded eterm then
        Option.map
          ~f:(fun req ->
            let t = Eval.simplify (Reduce.reduce_term (mk_app req [ eterm ])) in
            t)
          (Specifications.get_requires p.psi_target.PMRS.pvar)
      else None

let filter_elims all_subs t =
  List.remove_consecutive_duplicates
    ~equal:(fun (x1, _) (x2, _) -> Terms.equal x1 x2)
    (List.filter_map all_subs ~f:(fun (t_rec, t_scalar) ->
         match
           Set.to_list (Set.inter (Analysis.free_variables t_rec) (Analysis.free_variables t))
         with
         | [] -> None
         | x :: _ -> Some (mk_var x, t_scalar)))

let make ?(force_replace_off = false) ~(p : psi_def) ~(term_state : term_state) ~(lifting : lifting)
    (tset : TermSet.t) : equation list * lifting =
  let proj_to_non_lifting = Lifting.proj_to_non_lifting p in
  (* Compute a first set of constraints E(t) : spec o repr (t) = target (t) *)
  let eqns =
    let fold_f eqns t =
      let lhs = compute_lhs p t in
      let rhs = compute_rhs ~force_replace_off p t in
      eqns @ [ (t, lhs, rhs) ]
    in
    Set.fold ~init:[] ~f:fold_f tset
  in
  (* Compute the recursion eliminations as well as related invariants that need applying. *)
  let all_subs, invariants =
    Expand.subst_recursive_calls p
      (List.concat (List.map ~f:(fun (_, lhs, rhs) -> [ lhs; rhs ]) eqns))
  in
  (* Substitution function: some substitution opportunities appear after a first pass of subsitution
     followed by a lambda-reduction.
  *)
  let applic x = x |> substitution all_subs |> Reduce.reduce_term |> substitution all_subs in
  if Set.length invariants > 0 then
    Log.verbose
      Fmt.(
        fun frmt () ->
          pf frmt "Imᵢₙᵥ: @[<hov 2>%a@]"
            (styled `Italic (list ~sep:comma pp_term))
            (Set.elements invariants))
  else Log.verbose_msg "No image invariants.";
  let pure_eqns, lifting =
    let f (eqns_accum, lifting) (eterm, lhs, rhs) =
      (* Compute the lhs and rhs of the equations. *)
      let lhs' = Reduce.reduce_term (applic lhs) and rhs' = Reduce.reduce_term (applic rhs) in
      let rhs' =
        match proj_to_non_lifting with
        | Some func_term -> Reduce.reduce_term (mk_app func_term [ rhs' ])
        | None -> rhs'
      in
      let lhs'', rhs'' =
        if !Config.simplify_eqns then (Eval.simplify lhs', Eval.simplify rhs') else (lhs', rhs')
      in
      (* Filter the relevant part of the recursion elimination substitution, and only retain a map
             from recursive-typed variable to scalar variables replacing calls.
      *)
      let eelim = filter_elims all_subs eterm in
      (* Get the precondition, from the lemmas in the term state, *)
      let precond = compute_preconds ~p ~term_state applic eterm in
      let lifting' =
        let eprecond =
          match invar invariants lhs'' rhs'' with
          | Some im_f -> (
              match precond with Some pl -> Some (mk_bin And im_f pl) | None -> Some im_f)
          | None -> precond
        in
        Lifting.deduce_lifting_expressions ~p lifting eprecond lhs'' rhs''
      in
      (* Replace the boxed expressions of the lifting. *)
      let rhs'' =
        rhs' |> Lifting.replace_boxed_expressions ~p lifting' |> Reduce.reduce_term ~unboxing:true
      in

      (* If possible project equation of tuples into tuple of equations. *)
      let projs = projection_eqns lhs'' rhs'' in
      ( eqns_accum
        @ List.map
            ~f:(fun (elhs, erhs) ->
              (* Select the relevant preconditions. *)
              let eprecond =
                match invar invariants elhs erhs with
                | Some im_f -> (
                    match precond with Some pl -> Some (mk_bin And im_f pl) | None -> Some im_f)
                | None -> precond
              in
              { eterm; eprecond; elhs; erhs; eelim })
            projs,
        lifting' )
    in
    List.fold ~init:([], lifting) ~f eqns
  in
  Log.verbose (fun f () ->
      let print_less = List.take pure_eqns !Config.pp_eqn_count in
      Fmt.(
        pf f "Equations (%i) @." (Set.length tset);
        List.iter ~f:(fun eqn -> Fmt.pf f "@[%a@]@." pp_equation eqn) print_less));
  (* Phase 2 of the equation generation.
     Generate the equations corresponding to the lifting constraints. *)
  let lifting_eqns =
    let constraint_of_lift_expr ((i, t0), elhs) =
      let t0_rhs = compute_rhs p t0 in
      let erhs =
        mk_sel t0_rhs i |> Reduce.reduce_term
        |> Lifting.replace_boxed_expressions ~p lifting
        |> Reduce.reduce_term ~unboxing:true
      in
      let precond = compute_preconds ~p ~term_state (fun x -> x) t0 in
      let eprecond =
        match invar invariants elhs erhs with
        | Some im_f -> (
            match precond with Some pl -> Some (mk_bin And im_f pl) | None -> Some im_f)
        | None -> precond
      in
      let eelim = filter_elims all_subs t0 in
      { eterm = t0; elhs; erhs; eprecond; eelim }
    in
    List.map ~f:constraint_of_lift_expr lifting.tmap
  in
  Log.verbose (fun f () ->
      let print_less = List.take lifting_eqns !Config.pp_eqn_count in
      Fmt.(
        pf f "Lifting constraints (%i) @." (List.length lifting_eqns);
        List.iter ~f:(fun eqn -> Fmt.pf f "@[%a@]@." pp_equation eqn) print_less));

  match List.find ~f:(fun eq -> not (check_equation ~p eq)) (pure_eqns @ lifting_eqns) with
  | Some not_pure ->
      Log.error_msg Fmt.(str "Not pure: %a" pp_equation not_pure);
      failwith "Equation not pure."
  | None -> (pure_eqns @ lifting_eqns, lifting)

let revert_projs (orig_xi : VarSet.t)
    (projections : (int, variable list, Int.comparator_witness) Map.t)
    (soln : (string * variable list * term) list) : (string * variable list * term) list =
  (* Helper functions *)
  let find_soln s = List.find_exn ~f:(fun (s', _, _) -> String.equal s.vname s') soln in
  let join_bodies main_args first_body rest =
    let f accum (_, args, body) =
      let subst =
        match List.zip args main_args with
        | Ok l -> List.map l ~f:(fun (v1, v2) -> (mk_var v1, mk_var v2))
        | Unequal_lengths -> failwith "Projections should have same number of arguments."
      in
      accum @ [ substitution subst body ]
    in
    let tuple_elts = List.fold ~f ~init:[ first_body ] rest in
    mk_tup tuple_elts
  in
  (* Helper sets *)
  let all_proj_names, xi_projected =
    let x0 = Map.to_alist projections in
    let x1 = List.concat (List.map ~f:(fun (id, l) -> List.map ~f:(fun e -> (id, e)) l) x0) in
    ( List.map ~f:(fun (_, v) -> v.vname) x1,
      VarSet.of_list (List.filter_map ~f:(fun (id, _) -> VarSet.find_by_id orig_xi id) x1) )
  in
  let _, rest =
    let f (s, _, _) = List.mem all_proj_names ~equal:String.equal s in
    List.partition_tf ~f soln
  in
  (* Build for each xi projected *)
  let new_xi_solns =
    let f xi =
      let vls = Map.find_exn projections xi.vid in
      let solns = List.map ~f:find_soln vls in
      match solns with
      | [] -> failwith "revert_projs : failed to find an expected solution."
      | [ (_, args, body) ] -> (xi.vname, args, body) (* This should not happen though. *)
      | (_, args1, body1) :: tl -> (xi.vname, args1, join_bodies args1 body1 tl)
    in
    List.map ~f (Set.elements xi_projected)
  in
  rest @ new_xi_solns

let free_vars_of_equations (sys_eq : equation list) : VarSet.t =
  VarSet.union_list
    (List.concat_map
       ~f:(fun eqn ->
         [ Analysis.free_variables eqn.elhs; Analysis.free_variables eqn.erhs ]
         @ Option.(to_list (map ~f:Analysis.free_variables eqn.eprecond)))
       sys_eq)

(* ============================================================================================= *)
(*                               SOLVING SYSTEMS OF EQUATIONS                                    *)
(* ============================================================================================= *)
type partial_soln = (string * variable list * term) list

let pp_partial_soln (f : Formatter.t) soln =
  Fmt.(
    list ~sep:comma (fun fmrt (s, args, bod) ->
        match args with
        | [] -> pf fmrt "@[<hov 2>@[%s@] = @[%a@]@]" s pp_term bod
        | _ ->
            pf fmrt "@[<hov 2>@[%s(%a)@] = @[%a@]@]" s (list ~sep:comma Variable.pp) args pp_term
              bod))
    f soln

let combine ?(verb = false) prev_sol new_response =
  Either.(
    match (prev_sol, new_response) with
    | First soln, (resp, First soln') ->
        if verb then
          Log.debug
            Fmt.(
              fun frmt () -> pf frmt "@[Partial solution:@;@[<hov 2>%a@]@]" pp_partial_soln soln');
        (resp, First (soln @ soln'))
    | Second ctexs, (resp, Second ctexs') -> (resp, Second (ctexs @ ctexs'))
    | Second ctexs, (resp, First _) | First _, (resp, Second ctexs) -> (resp, Second ctexs))

let solve_syntactic_definitions (unknowns : VarSet.t) (eqns : equation list) =
  (* Are all arguments free? *)
  let ok_rhs_args _args =
    let arg_vars = VarSet.union_list (List.map ~f:Analysis.free_variables _args) in
    Set.is_empty (Set.inter unknowns arg_vars)
  in
  (* Is lhs, args a full definition of the function? *)
  let ok_lhs_args lhs args =
    let argv = List.map args ~f:ext_var_or_none in
    let argset = VarSet.of_list (List.concat (List.filter_opt argv)) in
    if
      List.for_all ~f:Option.is_some argv
      && Set.is_empty (Set.diff (Analysis.free_variables lhs) argset)
    then
      let args = List.filter_opt argv in
      if List.length (List.concat args) = Set.length argset then Some args else None
    else None
  in
  (* Make a function out of lhs of equation constraint using args. *)
  let mk_lam lhs args =
    let pre_subst =
      List.map args ~f:(fun arg_tuple ->
          let t =
            match arg_tuple with
            | [ v ] -> Variable.vtype_or_new v
            | l -> RType.TTup (List.map ~f:Variable.vtype_or_new l)
          in
          let v = Variable.mk ~t:(Some t) (Alpha.fresh ()) in
          match arg_tuple with
          | [ arg ] -> (v, [ (mk_var arg, mk_var v) ])
          | l -> (v, List.mapi l ~f:(fun i arg -> (mk_var arg, mk_sel (mk_var v) i))))
    in
    let new_args, subst = List.unzip pre_subst in
    (new_args, Reduce.reduce_term (substitution (List.concat subst) lhs))
  in
  (* Find syntactic definiitons, and check there is no conflict between definitions. *)
  let full_defs, other_eqns =
    let f (defs, other_eqns) eqn =
      match (eqn.eprecond, eqn.erhs.tkind) with
      | _, TApp ({ tkind = TVar unknown; _ }, args)
        when Set.mem unknowns unknown && ok_rhs_args args -> (
          match ok_lhs_args eqn.elhs args with
          | Some argv ->
              let lam_args, lam_body = mk_lam eqn.elhs argv in
              (Map.add_multi defs ~key:unknown ~data:(lam_args, lam_body, eqn), other_eqns)
          | None -> (defs, eqn :: other_eqns))
      | _ -> (defs, eqn :: other_eqns)
    in
    let possible_defs, others = List.fold ~init:(Map.empty (module Variable), []) ~f eqns in
    let validated_defs, others' =
      List.partition_map (Map.to_alist possible_defs) ~f:(fun (unknown, exprs) ->
          match exprs with
          | [] -> Either.Second []
          | [ (lam_args, lam_body, _) ] -> Either.First (unknown, (lam_args, lam_body))
          | (lam_args1, lam_body1, _) :: tl ->
              (* Check that all syntactic definitions agree *)
              let all_equal =
                List.for_all tl ~f:(fun (lam_args2, lam_body2, _) ->
                    (* Check (fun args2 -> body2) args1 = body1 *)
                    Terms.equal lam_body1
                      (Reduce.reduce_term
                         (mk_app
                            (mk_fun (List.map ~f:(fun v -> FPatVar v) lam_args2) lam_body2)
                            (List.map ~f:mk_var lam_args1))))
              in
              if all_equal then Either.First (unknown, (lam_args1, lam_body1))
              else Either.Second (List.map ~f:(fun (_, _, eqn) -> eqn) exprs))
    in
    (validated_defs, List.rev others @ List.concat others')
  in
  let resolved = VarSet.of_list (List.map ~f:Utils.first full_defs) in
  let new_eqns =
    let substs =
      List.map full_defs ~f:(fun (x, (lhs_args, lhs_body)) ->
          let t, _ = infer_type (mk_fun (List.map ~f:(fun x -> FPatVar x) lhs_args) lhs_body) in
          (mk_var x, t))
    in
    List.map other_eqns ~f:(fun eqn ->
        let new_lhs = Reduce.reduce_term (substitution substs eqn.elhs) in
        let new_rhs = Reduce.reduce_term (substitution substs eqn.erhs) in
        { eqn with elhs = new_lhs; erhs = new_rhs })
  in
  let partial_soln =
    List.map ~f:(fun (x, (lhs_args, lhs_body)) -> (x.vname, lhs_args, lhs_body)) full_defs
  in
  if List.length partial_soln > 0 then
    Log.debug_msg Fmt.(str "Syntactic definition:@;@[<hov 2>%a@]" pp_partial_soln partial_soln);
  (partial_soln, Set.diff unknowns resolved, new_eqns)

let synthfuns_of_unknowns ?(bools = false) ?(eqns = []) ?(ops = OpSet.empty) (unknowns : VarSet.t) =
  let xi_formals (xi : variable) : sorted_var list * sygus_sort =
    let tv = Variable.vtype_or_new xi in
    let targs, tout = RType.fun_typ_unpack tv in
    (sorted_vars_of_types targs, sort_of_rtype tout)
  in
  let f xi =
    let args, ret_sort = xi_formals xi in
    let guess =
      if !Config.optimize_grammars then
        Grammars.make_guess
          (List.map ~f:(fun eqn -> (eqn.eterm, eqn.eprecond, eqn.elhs, eqn.erhs)) eqns)
          xi
      else None
    in
    let grammar =
      Grammars.generate_grammar ~guess ~bools ~special_const_prod:false ops args ret_sort
    in
    CSynthFun (xi.vname, args, ret_sort, grammar)
  in
  List.map ~f (Set.elements unknowns)

let constraints_of_eqns (eqns : equation list) : command list =
  let detupled_equations =
    let f eqn =
      let eqs = projection_eqns eqn.elhs eqn.erhs in
      List.map ~f:(fun (_l, _r) -> (eqn.eprecond, _l, _r)) eqs
    in
    List.concat (List.map ~f eqns)
  in
  let eqn_to_constraint (pre, lhs, rhs) =
    match pre with
    | Some precondition -> CConstraint (sygus_of_term Terms.(~!precondition || lhs == rhs))
    | None -> CConstraint (sygus_of_term Terms.(lhs == rhs))
  in
  List.map ~f:eqn_to_constraint detupled_equations

let aux_solve (gen_only : bool) (unknowns : VarSet.t) (eqns : equation list) =
  let free_vars, all_operators, has_ite =
    let f (fvs, ops, hi) eqn =
      let precond, lhs, rhs = (eqn.eprecond, eqn.elhs, eqn.erhs) in
      let set' =
        VarSet.union_list
          Analysis.
            [
              free_variables lhs;
              free_variables rhs;
              Option.value_map precond ~f:free_variables ~default:VarSet.empty;
            ]
      in
      Analysis.
        ( Set.union fvs set',
          Set.union ops (Set.union (operators_of lhs) (operators_of rhs)),
          hi || has_ite lhs || has_ite rhs )
    in
    let fvs, ops, hi = List.fold eqns ~f ~init:(VarSet.empty, Set.empty (module Operator), false) in
    (Set.diff fvs unknowns, ops, hi)
  in
  (* Prepare commands *)
  let logic =
    let base_logic = logic_of_operators all_operators in
    let needs_dt =
      List.exists
        ~f:(fun v -> requires_dt_theory (Variable.vtype_or_new v))
        (Set.elements free_vars @ Set.elements unknowns)
    in
    if needs_dt then dt_extend_base_logic base_logic else base_logic
  in
  let synth_objs = synthfuns_of_unknowns ~bools:has_ite ~eqns ~ops:all_operators unknowns in
  let set_logic = CSetLogic logic in
  let sort_decls = declare_sorts_of_vars free_vars in
  let var_decls = List.map ~f:declaration_of_var (Set.elements free_vars) in
  let constraints = constraints_of_eqns eqns in
  let extra_defs =
    (if Set.mem all_operators (Binary Max) then [ max_definition ] else [])
    @ if Set.mem all_operators (Binary Min) then [ min_definition ] else []
  in
  let commands =
    set_logic :: (extra_defs @ sort_decls @ synth_objs @ var_decls @ constraints @ [ CCheckSynth ])
  in
  (* Call the solver. *)
  let handle_response (resp : solver_response) =
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
    | RSuccess resps ->
        let soln = List.map ~f:parse_synth_fun resps in
        (resp, Either.First soln)
    | RInfeasible -> (RInfeasible, Either.Second [])
    | RFail -> (RFail, Either.Second [])
    | RUnknown -> (RUnknown, Either.Second [])
  in
  if !Config.generate_benchmarks then
    Syguslib.Solvers.commands_to_file commands
      (* Assuming gen_only true only for unrealizable problems. *)
      (Config.new_benchmark_file ~hint:(if gen_only then "unrealizable_" else "") ".sl");
  (* Call the solver on the generated file. *)
  if not gen_only then
    let t, r = SygusInterface.SygusSolver.solve_commands commands in
    (Lwt.map (function Some resp -> handle_response resp | None -> (RFail, Either.Second [])) t, r)
  else Lwt.task () |> fun (_, r) -> (Lwt.return (RFail, Either.Second []), r)

let solve_eqns (unknowns : VarSet.t) (eqns : equation list) :
    solver_response * (partial_soln, Counterexamples.unrealizability_ctex list) Either.t =
  let lwt_tasks =
    [
      (* Task 1 : checking unrelizability. *)
      (fun () ->
        if !Config.check_unrealizable then (
          match Counterexamples.check_unrealizable unknowns eqns with
          | [] ->
              (* It not infeasible, sleep for timeout duration. *)
              let* () = Lwt_unix.sleep !Config.wait_parallel_tlimit in
              Lwt.return (RFail, Either.Second [])
          | _ as ctexs ->
              if !Config.generate_benchmarks then ignore (aux_solve false unknowns eqns);
              if !Config.check_unrealizable_smt_unsatisfiable then
                Counterexamples.smt_unsatisfiability_check unknowns eqns;
              Lwt.return (RInfeasible, Either.Second ctexs))
        else
          let* () = Lwt_unix.sleep !Config.wait_parallel_tlimit in
          Lwt.return (RFail, Either.Second []));
      (* Task 2 : solving system of equations. *)
      (fun () ->
        let t, r = aux_solve false unknowns eqns in
        Lwt.wakeup r 0;
        t);
    ]
  in
  Log.debug_msg Fmt.(str "Solving for %a." VarSet.pp unknowns);
  Lwt_main.run (Lwt.pick (List.map ~f:(fun x -> x ()) lwt_tasks))

let solve_eqns_proxy (unknowns : VarSet.t) (eqns : equation list) =
  if !Config.use_syntactic_definitions then
    let partial_soln, new_unknowns, new_eqns = solve_syntactic_definitions unknowns eqns in
    if Set.length new_unknowns > 0 then
      combine (Either.First partial_soln) (solve_eqns new_unknowns new_eqns)
    else (RSuccess [], Either.First partial_soln)
  else solve_eqns unknowns eqns

(* Solve the trivial equations first, avoiding the overhead from the
   sygus solver.
*)
let solve_constant_eqns (unknowns : VarSet.t) (eqns : equation list) =
  let constant_soln, other_eqns =
    let f eqn =
      match eqn.erhs.tkind with
      | TVar x when Set.mem unknowns x ->
          if Analysis.is_constant eqn.elhs then Either.first (x, eqn.elhs) else Either.Second eqn
      | _ -> Either.Second eqn
    in
    List.partition_map ~f eqns
  in
  let resolved = VarSet.of_list (List.map ~f:Utils.first constant_soln) in
  let new_eqns =
    let substs = List.map ~f:(fun (x, lhs) -> (mk_var x, lhs)) constant_soln in
    List.map other_eqns ~f:(fun eqn ->
        { eqn with elhs = substitution substs eqn.elhs; erhs = substitution substs eqn.erhs })
  in
  let partial_soln = List.map ~f:(fun (x, lhs) -> (x.vname, [], lhs)) constant_soln in
  if List.length partial_soln > 0 then
    Log.debug Fmt.(fun fmt () -> pf fmt "@[Constant:@;@[<hov 2>%a@]@]" pp_partial_soln partial_soln);
  (partial_soln, Set.diff unknowns resolved, new_eqns)

let split_solve partial_soln (unknowns : VarSet.t) (eqns : equation list) =
  (* If an unknown depends only on itself, it can be split from the rest *)
  let split_eqn_systems =
    let f (l, u, e) xi =
      (* Separate in set of equation where u appears and rest *)
      let eqn_u, rest =
        List.partition_tf e ~f:(fun eqn ->
            let fv =
              Set.union (Analysis.free_variables eqn.elhs) (Analysis.free_variables eqn.erhs)
            in
            Set.mem fv xi)
      in
      let eqn_only_u, eqn_u =
        List.partition_tf eqn_u ~f:(fun eqn ->
            let fv =
              Set.union (Analysis.free_variables eqn.elhs) (Analysis.free_variables eqn.erhs)
            in
            Set.is_empty (Set.inter fv (Set.diff unknowns (VarSet.singleton xi))))
      in
      match eqn_u with
      | [] ->
          Log.debug_msg Fmt.(str "Synthesize %s independently." xi.vname);
          (l @ [ (VarSet.singleton xi, eqn_only_u) ], u, rest)
      | _ -> (l, Set.add u xi, e)
    in
    let sl, u, e = List.fold (Set.elements unknowns) ~f ~init:([], VarSet.empty, eqns) in
    sl @ [ (u, e) ]
  in
  let solve_eqn_aux (unknowns, equations) =
    if Set.length unknowns > 0 then [ solve_eqns_proxy unknowns equations ] else []
  in
  (* First solve by mapping to subsystems. Will be done in parallel. *)
  let solved = List.concat_map split_eqn_systems ~f:solve_eqn_aux in
  (* Then combine the solutions. *)
  List.fold solved ~init:(RSuccess [], Either.First partial_soln) ~f:(fun (_, prev_sol) r ->
      combine prev_sol r)

let solve_stratified (unknowns : VarSet.t) (eqns : equation list) =
  let psol, u, e =
    if !Config.use_syntactic_definitions then
      let c_soln, no_c_unknowns, no_c_eqns = solve_constant_eqns unknowns eqns in
      let partial_soln', new_unknowns, new_eqns =
        solve_syntactic_definitions no_c_unknowns no_c_eqns
      in
      (c_soln @ partial_soln', new_unknowns, new_eqns)
    else ([], unknowns, eqns)
  in
  if !Config.split_solve_on then split_solve psol u e
  else
    Either.(
      match solve_eqns u e with
      | resp, First soln -> (resp, First (psol @ soln))
      | resp, Second ctexs -> (resp, Second ctexs))

(* ============================================================================================= *)
(*                               PREPROCESSING SYSTEM OF EQUATIONS                               *)
(* ============================================================================================= *)

type preprocessing_action_result = {
  pre_unknowns : VarSet.t;
  pre_equations : equation list;
  pre_postprocessing :
    solver_response * (partial_soln, Counterexamples.unrealizability_ctex list) Either.t ->
    solver_response * (partial_soln, Counterexamples.unrealizability_ctex list) Either.t;
}
(** A preprocessing action should return a new system of equations,
   and optionally a new set of unknowns together with a postprocessing function. *)

(** An empty preprocessing action. *)
let preprocess_none u eqs =
  { pre_unknowns = u; pre_equations = eqs; pre_postprocessing = (fun x -> x) }

(** Preprocessing action that projects each unknown returning a tuple into a tuple
  of unknowns and change the equations accordingly.
  Postprocessing consist of rebuilding the tuples.
*)
let preprocess_detuple (unknowns : VarSet.t) (eqns : equation list) : preprocessing_action_result =
  let pre_unknowns, projections = proj_functions unknowns in
  let pre_equations = proj_and_detuple_eqns projections eqns in
  let pre_postprocessing (resp, soln) =
    Either.(
      match soln with
      | First soln ->
          let soln =
            if Map.length projections > 0 then revert_projs unknowns projections soln else soln
          in
          (resp, First soln)
      | Second ctexs -> (resp, Second ctexs))
  in
  { pre_unknowns; pre_equations; pre_postprocessing }

(** Preprocessing action that transforms constraints with conditionals into sets of constraints
  where conditions have been moved in the precondition of the constraint.
*)
let preprocess_deconstruct_if (unknowns : VarSet.t) (eqns : equation list) :
    preprocessing_action_result =
  let and_opt precond t =
    match precond with Some pre -> Some (mk_bin And pre t) | None -> Some t
  in
  let pre_equations =
    let rec split_if eqn =
      match eqn.erhs.tkind with
      | TIte (rhs_c, rhs_bt, rhs_bf) ->
          if Set.is_empty (Set.inter (Analysis.free_variables rhs_c) unknowns) then
            match eqn.elhs.tkind with
            | TIte (lhs_c, lhs_bt, lhs_bf) when Terms.equal rhs_c lhs_c ->
                split_if
                  { eqn with eprecond = and_opt eqn.eprecond rhs_c; elhs = lhs_bt; erhs = rhs_bt }
                @ split_if
                    {
                      eqn with
                      eprecond = and_opt eqn.eprecond (mk_un Not rhs_c);
                      elhs = lhs_bf;
                      erhs = rhs_bf;
                    }
            | _ ->
                split_if { eqn with eprecond = and_opt eqn.eprecond rhs_c; erhs = rhs_bt }
                @ split_if
                    { eqn with eprecond = and_opt eqn.eprecond (mk_un Not rhs_c); erhs = rhs_bf }
          else [ eqn ]
      | _ -> [ eqn ]
    in
    List.concat_map ~f:split_if eqns
  in
  { pre_unknowns = unknowns; pre_equations; pre_postprocessing = (fun x -> x) }

(** For each equation, search within the system of equations whether there is another
    equation that constrains a subexpression.
*)
let preprocess_factor_subexpressions (unknowns : VarSet.t) (eqns : equation list) :
    preprocessing_action_result =
  let finder sube = List.find ~f:(fun e' -> Terms.equal e'.erhs sube) eqns in
  let precond_compat eqn1 eqn2 = Option.equal Terms.equal eqn1.eprecond eqn2.eprecond in
  let pre_equations =
    let replace_rhs_subexpr eqn1 =
      let case _ t =
        Option.bind (finder t) ~f:(fun eqn2 ->
            if precond_compat eqn1 eqn2 then Some eqn2.elhs else None)
      in
      let erhs' = transform_at_depth 1 ~case eqn1.erhs in
      { eqn1 with erhs = erhs' }
    in
    List.map ~f:replace_rhs_subexpr eqns
  in
  { pre_unknowns = unknowns; pre_equations; pre_postprocessing = (fun x -> x) }

(* ============================================================================================= *)
(*                              MAIN ENTRY POINT                                                 *)
(* ============================================================================================= *)

(** Main entry point: solve a system of equations by synthesizing the unknowns. Returns either a
  solution as a list of implementations for the unknowns (a triple of unknown name, arguments of a
  function and body of a function) or a list of unrealizability counterexamples.
*)
let solve ~(p : psi_def) (eqns : equation list) :
    solver_response * (partial_soln, Counterexamples.unrealizability_ctex list) Either.t =
  let unknowns = p.psi_target.psyntobjs in
  let preprocessing_actions =
    [
      preprocess_deconstruct_if;
      (if !Config.detupling_on then preprocess_detuple else preprocess_none);
      preprocess_factor_subexpressions;
    ]
  in
  let soln_final =
    (* Apply the preprocessing actions, and construct the postprocessing in reverse. *)
    let unknowns', eqns', postprocessing_actions =
      List.fold preprocessing_actions ~init:(unknowns, eqns, [])
        ~f:(fun (u, e, post_acts) pre_act ->
          let ppact = pre_act u e in
          (ppact.pre_unknowns, ppact.pre_equations, ppact.pre_postprocessing :: post_acts))
    in
    (* Apply the postprocessing after solving. *)
    List.fold postprocessing_actions ~init:(solve_stratified unknowns' eqns')
      ~f:(fun partial_solution post_act -> post_act partial_solution)
  in
  Either.(
    match soln_final with
    | _, First soln ->
        Utils.Log.debug
          Fmt.(fun fmt () -> pf fmt "@[<hov 2>Solution found: @;%a@]" (box pp_partial_soln) soln)
    | _ -> ());
  soln_final
