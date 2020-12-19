open Base
open Lang.Term
open Lang
open AState
open Utils
open SmtInterface
open Smtlib.Solvers

let check_solution ~(p : psi_def) (t, u : TermSet.t * TermSet.t) (soln : (string * variable list * term) list) =
  let xi_set = p.target.pparams in
  let xi_substs =
    let f (name, args, body) =
      match VarSet.find_by_name xi_set name with
      | Some xi -> [Term.mk_var xi, mk_fun (List.map ~f:(fun x -> PatVar x) args) body]
      | None -> []
    in List.concat (List.map ~f soln)
  in
  let target_inst =
    PMRS.subst_rule_rhs xi_substs ~p:p.target
  in
  let term_set, _ =
    let expansion_step (ts, us : TermSet.t * TermSet.t) : TermSet.t * TermSet.t =
      let l = List.map ~f:(Expand.maximal p) (Set.elements us) in
      List.fold l ~init:(ts, TermSet.empty)
        ~f:(fun (ts, us) (t', u') -> Set.union ts t', Set.union us u')
    in
    Utils.repeat Config.num_expansions_check expansion_step ~init:(t,u)
  in
  let equations = Equations.make ~p:{ p with target=target_inst} term_set in
  let smt_equations =
    let f (lhs, rhs) = mk_eq (smt_of_term lhs) (smt_of_term rhs) in
    List.map ~f equations
  in
  let formula = mk_not (mk_simple_app "and" smt_equations) in
  let free_vars =
    let f fv (lhs, rhs) =
      Set.union fv (Set.union (Analysis.free_variables lhs) (Analysis.free_variables rhs))
    in
    List.fold ~f ~init:VarSet.empty equations
  in
  let vardecls = decls_of_vars free_vars in
  let solver = make_z3_solver () in
  declare_all solver vardecls;
  smt_assert solver formula;
  let _ =
    match check_sat solver with
    | Sat ->
      (match get_model solver with
       | SExps sl ->
         Fmt.(pf stdout "Model:%a@." (list Sexp.pp_hum) sl);
         Some sl
       | _ -> None)
    | Unsat -> None
    | Unknown -> failwith "Unknown"
    | Error _ -> failwith "Error"
    | SExps _ -> failwith "unexpected response"
  in
  close_solver solver;
  ()
