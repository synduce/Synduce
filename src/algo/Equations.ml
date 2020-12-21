open Base
open Lang
open Lang.Term
open AState
open Syguslib.Sygus
open SygusInterface
module SmtI = SmtInterface


let identify_rcalls (lam : variable) (t : term) =
  let join = Set.union in
  let case _ t =
    match t.tkind with
    (* lam x *)
    | TApp({tkind = TVar lam';_}, [single_arg]) when Variable.(lam' = lam) ->
      (match single_arg.tkind with
       | TVar x -> Some (VarSet.singleton x)
       | _ -> None)
    | _ -> None
  in
  reduce ~init:VarSet.empty ~case ~join t



type equation = term * term * term


let check_equation ~(p : psi_def) (_, lhs, rhs : equation) : bool =
  match Expand.nonreduced_terms p lhs, Expand.nonreduced_terms p rhs with
  | [], [] -> true
  | _ -> false


let make ~(p : psi_def) (tset : TermSet.t) : equation list =
  let fsymb = p.orig.pmain_symb and gsymb = p.target.pmain_symb in
  (* Replace recursive calls to r(x) *)
  let rcalls, eqns =
    let fold_f (rcalled_vars, eqns) t =
      let lhs = Expand.replace_nonreduced_by_main p (PMRS.reduce p.orig t) in
      let rhs = Expand.replace_nonreduced_by_main p (PMRS.reduce p.target t) in
      let f_x = identify_rcalls fsymb lhs in
      let g_x = identify_rcalls gsymb rhs in
      VarSet.union_list [rcalled_vars; f_x; g_x], (t, lhs, rhs) :: eqns
    in
    Set.fold ~init:(VarSet.empty, []) ~f:fold_f tset
  in
  let all_subs =
    let f var =
      let scalar_term = mk_composite_scalar !AState.alpha in
      [mk_app (mk_var fsymb) [mk_var var], scalar_term;
       mk_app (mk_var gsymb) [mk_var var], scalar_term]
    in
    List.concat (List.map ~f (Set.elements rcalls))
  in
  let pure_eqns =
    let f (t, lhs, rhs) =
      let applic x = Analysis.reduce_term (substitution all_subs x) in
      t, applic lhs, applic rhs
    in
    List.map ~f eqns
  in
  if List.for_all ~f:(check_equation ~p) pure_eqns then
    pure_eqns
  else
    failwith "Equation not pure."

(* ============================================================================================= *)
(*                               SOLVING SYSTEMS OF EQUATIONS                                    *)
(* ============================================================================================= *)
let constraints_of_eqns (eqns : equation list) : command list =
  let eqn_to_constraint (_, lhs, rhs) =
    CConstraint (SyApp(IdSimple "=", [sygus_of_term lhs; sygus_of_term rhs]))
  in
  List.map ~f:eqn_to_constraint eqns


let synthfuns_of_unknowns ?(ops = OpSet.empty) (unknowns : VarSet.t) : command list =
  let decompose_xi_args (xi : variable) =
    match Variable.vtype_or_new xi with
    | RType.TFun(TTup(targs), tres) -> sorted_vars_of_types targs, sort_of_rtype tres
    | RType.TFun(targ, tres) -> sorted_vars_of_types [targ], sort_of_rtype tres
    | t -> [], sort_of_rtype t
  in
  let f xi =
    let args, ret_sort = decompose_xi_args xi in
    let grammar = Grammars.generate_grammar ops args ret_sort in
    CSynthFun (xi.vname, args, ret_sort, grammar)
  in
  List.map ~f (Set.elements unknowns)


let solve ~(p : psi_def) (eqns : equation list) =
  let free_vars, all_operators =
    let f (fvs, ops) (_, lhs, rhs) =
      VarSet.union_list [fvs; Analysis.free_variables lhs; Analysis.free_variables rhs],
      Set.union ops (Set.union (Grammars.operators_of lhs) (Grammars.operators_of rhs))
    in
    let fvs, ops = List.fold eqns ~f ~init:(VarSet.empty, Set.empty (module Operator)) in
    Set.diff fvs p.target.pparams, ops
  in
  (* Commands *)
  let set_logic = CSetLogic(Grammars.logic_of_operator all_operators) in
  let synth_objs = synthfuns_of_unknowns ~ops:all_operators p.target.pparams in
  let var_decls = List.map ~f:declaration_of_var (Set.elements free_vars) in
  let constraints = constraints_of_eqns eqns in
  let extra_defs =
    (if Set.mem all_operators (Binary Max) then [max_definition] else []) @
    (if Set.mem all_operators (Binary Min) then [min_definition] else [])
  in
  let commands =
    set_logic :: (extra_defs @ synth_objs @ var_decls @ constraints @ [CCheckSynth])
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
      fname, args, body
    in
    match resp with
    | RSuccess (resps) -> resp, Some (List.map ~f:parse_synth_fun resps)
    | RInfeasible -> RInfeasible, None
    | RFail -> RFail, None
    | RUnknown -> RUnknown, None
  in
  match Syguslib.Solvers.CVC4.solve_commands commands with
  | Some resp -> handle_response resp
  | None -> RFail, None

