open Base
open Lang.Term
open Lang
open AState
open Utils
open SmtInterface
open Smtlib.Solvers

let constr_eqn (_, pre, lhs, rhs) =
  let rec mk_eqn (lhs, rhs) =
    let lhs, rhs = Reduce.reduce_term lhs, Reduce.reduce_term rhs in
    match lhs.tkind, rhs.tkind with
    | TTup ltl, TTup rtl ->
      (match List.zip ltl rtl with
       | Ok rt_lt -> mk_assoc_and (List.map ~f:mk_eqn rt_lt)
       | _ -> failwith "Verification failed because unexpected tuple size.")
    | _ -> mk_eq (smt_of_term lhs) (smt_of_term rhs)
  in
  let eqn = mk_eqn (lhs, rhs) in
  match pre with
  | Some inv -> mk_or (mk_not (smt_of_term inv)) eqn
  | None -> eqn

(**
   `check_solution ~p (t,u) soln`

   Checks if ᴪ(p, soln) is valid, assuming `soln` has been synthesized using the term set `t` with
   expansion continuation `u`.
   Returns the set (t,u) for the next step in the ctex-guided refinement loop.
*)
let check_solution
    ?(use_naive = false)
    ~(p : psi_def)
    (t, u : TermSet.t * TermSet.t)
    (soln : (string * variable list * term) list) =
  (if use_naive then
     Log.info (fun f () -> Fmt.(pf f "<NAIVE> Check solution."))
   else
     Log.info (fun f () -> Fmt.(pf f "Checking solution...")));
  let start_time = Unix.gettimeofday () in
  let target_inst = Reduce.instantiate_with_solution p.target soln in
  let free_vars = VarSet.empty in
  let init_vardecls = decls_of_vars free_vars in
  let solver = make_z3_solver () in
  load_min_max_defs solver;
  let check_eqn has_sat eqn =
    let formula = mk_not eqn in
    spush solver;
    smt_assert solver formula;
    let x =
      match check_sat solver with
      | Sat -> true | _ -> has_sat
    in
    spop solver;
    x
  in
  let expand_and_check i (t0 : term) =
    let t_set, u_set =
      if use_naive then
        Expand.simple t0
      else
        Expand.to_maximally_reducible p t0
    in
    if Set.length t_set > 0 then
      let sys_eqns = Equations.make ~force_replace_off:true ~p:{ p with target=target_inst} t_set in
      let smt_eqns = List.map sys_eqns ~f:constr_eqn in
      let new_free_vars =
        let f fv (_, _, lhs, rhs) =
          Set.union fv (Set.union (Analysis.free_variables lhs) (Analysis.free_variables rhs))
        in
        Set.diff (List.fold ~f ~init:VarSet.empty sys_eqns) free_vars
      in
      spush solver;
      declare_all solver (decls_of_vars new_free_vars);
      let has_ctex = List.fold ~init:false ~f:check_eqn smt_eqns in
      spop solver;
      if has_ctex then true, t_set, u_set, i + 1 else false, TermSet.empty, u_set, i + 1
    else (* set is empty *)
      false, TermSet.empty, u_set, i
  in
  let rec find_ctex num_checks terms_to_expand =
    Log.verbose_msg Fmt.(str "Check %i." num_checks);
    if num_checks > !Config.num_expansions_check then None
    else match List.sort ~compare:term_size_compare (Set.elements terms_to_expand) with
      | [] -> None
      | hd :: tl ->
        let has_ctex, t_set, u_set, num_checks = expand_and_check num_checks hd in
        (if has_ctex then
           Some (Set.union t t_set, terms_to_expand)
         else
           let elts = Set.union u_set (TermSet.of_list tl) in
           find_ctex num_checks elts)
  in
  (* Declare all variables *)
  declare_all solver init_vardecls;
  (match find_ctex 0 t with
   | Some _ -> failwith "Synthesizer and solver disagree on solution. That's unexpected!"
   | None -> ());
  let ctex_or_none = find_ctex 0 u in
  close_solver solver;
  let elapsed = Unix.gettimeofday () -. start_time in
  Log.info (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
  ctex_or_none


(* Perform a bounded check of the solution *)
let bounded_check
    ~(p : psi_def)
    (soln : (string * variable list * term) list) =
  Log.info (fun f () -> Fmt.(pf f "Checking solution (bounded check)..."));
  let start_time = Unix.gettimeofday () in
  let target_inst = Reduce.instantiate_with_solution p.target soln in
  let free_vars = VarSet.empty in
  let init_vardecls = decls_of_vars free_vars in
  let solver = make_z3_solver () in
  load_min_max_defs solver;
  let check_eqn eqn =
    let formula = mk_not eqn in
    spush solver;
    smt_assert solver formula;
    let x =
      match check_sat solver with
      | Sat -> true | _ -> false
    in
    spop solver;
    x
  in
  let termset =
    Analysis.terms_of_max_depth !Config.check_depth !AState._theta
  in
  Log.debug_msg Fmt.(str "%i constraints." (List.length termset));
  let sys_eqns =
    Equations.make
      ~force_replace_off:true
      ~p:{ p with target=target_inst}
      (TermSet.of_list termset)
  in
  let smt_eqns = List.map sys_eqns ~f:(fun t -> t, constr_eqn t) in
  let new_free_vars =
    let f fv (_, _, lhs, rhs) =
      Set.union fv (Set.union (Analysis.free_variables lhs) (Analysis.free_variables rhs))
    in
    Set.diff (List.fold ~f ~init:VarSet.empty sys_eqns) free_vars
  in
  declare_all solver init_vardecls;
  spush solver;
  declare_all solver (decls_of_vars new_free_vars);
  let rec has_ctex _eqns =
    match _eqns with
    | [] -> None
    | (eqn, smt_eqn) :: tl ->
      if check_eqn smt_eqn then Some eqn
      else has_ctex tl
  in
  let ctex_or_none = has_ctex smt_eqns in
  spop solver;
  close_solver solver;
  let elapsed = Unix.gettimeofday () -. start_time in
  Log.info (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
  ctex_or_none
