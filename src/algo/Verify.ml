open Base
open Lang.Term
open Lang
open AState
open Utils
open SmtInterface
open Smtlib.Solvers


(**
   `check_solution ~p (t,u) soln`

   Checks if ᴪ(p, soln) is valid, assuming `soln` has been synthesized using the term set `t` with
   expansion continuation `u`.
   Returns the set (t,u) for the next step in the ctex-guided refinement loop.
*)
let check_solution ~(p : psi_def)
    (t, u : TermSet.t * TermSet.t)
    (soln : (string * variable list * term) list) =
  let target_inst = PMRS.instantiate_with_solution p.target soln in
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
  let expand_and_check (t0 : term) =
    let t_set, u_set = Expand.maximal p t0 in
    let sys_eqns = Equations.make ~p:{ p with target=target_inst} t_set in
    let smt_eqns = List.map sys_eqns
        ~f:(fun (_, pre, lhs, rhs) ->
            match pre with
            | Some _ -> failwith "TODO : invariants in verification."
            | None -> mk_eq (smt_of_term lhs) (smt_of_term rhs))
    in
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
    if has_ctex then true, t_set, u_set else false, TermSet.empty, u_set
  in
  let rec find_ctex num_checks terms_to_expand =
    if num_checks > Config.num_expansions_check then None
    else match terms_to_expand with
      | [] -> None
      | hd :: tl ->
        let has_ctex, t_set, u_set = expand_and_check hd in
        (if has_ctex then
           Some (Set.union t t_set, TermSet.of_list terms_to_expand)
         else find_ctex (num_checks + 1) (tl @ (Set.elements u_set)))
  in
  (* Declare all variables *)
  declare_all solver init_vardecls;
  let ctex_or_none = find_ctex 0 (Set.elements u) in
  close_solver solver;
  ctex_or_none
