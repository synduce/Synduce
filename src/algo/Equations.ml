open Base
open Lang
open Lang.Term
open AState
open Utils
open Syguslib.Sygus
open TermSynthesis


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



type equation = term * term


let check_equation ~(p : psi_def) (lhs, rhs : equation) : bool =
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
      VarSet.union_list [rcalled_vars; f_x; g_x], (lhs, rhs) :: eqns
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
    let f (lhs, rhs) =
      let applic x = Analysis.reduce_term (substitution all_subs x) in
      applic lhs, applic rhs
    in
    List.map ~f eqns
  in
  List.iter pure_eqns
    ~f:(fun (lhs, rhs) -> Log.debug_msg Fmt.(str "@[<hov 2>%a = %a@]" pp_term lhs pp_term rhs));
  if List.for_all ~f:(check_equation ~p) pure_eqns then
    pure_eqns
  else
    failwith "Equation not pure."


let solve ~(p : psi_def) (eqns : equation list) =
  let decompose_xi_args (xi : variable) =
    match Variable.vtype_or_new xi with
    | RType.TFun(TTup(targs), tres) -> sorted_vars_of_types targs, sort_of_rtype tres
    | RType.TFun(targ, tres) -> sorted_vars_of_types [targ], sort_of_rtype tres
    | t -> [], sort_of_rtype t
  in
  let set_logic = CSetLogic("DTLIA") in  (* TODO: deduce the logic from the terms. *)
  let synth_objs =
    let f xi =
      let args, ret_sort = decompose_xi_args xi in
      CSynthFun (xi.vname, args, ret_sort, None)
    in
    List.map ~f (Set.elements p.target.pparams)
  in
  let var_decls =
    let free_vars =
      let x =
        List.fold eqns ~init:VarSet.empty
          ~f:(fun s (lhs, rhs) ->
              VarSet.union_list [s; Analysis.free_variables lhs; Analysis.free_variables rhs])
      in
      Set.diff x p.target.pparams
    in
    List.map ~f:declaration_of_var (Set.elements free_vars)
  in
  let constraints =
    let eqn (lhs, rhs) =
      CConstraint (SyApp(IdSimple "=", [sygus_of_term lhs; sygus_of_term rhs]))
    in
    List.map ~f:eqn eqns
  in
  let extra_defs =
    [max_definition; min_definition]
  in
  let commands =
    set_logic :: (extra_defs @ synth_objs @ var_decls @ constraints @ [CCheckSynth])
  in
  let _ =
    Syguslib.Solvers.CVC4.solve_commands commands
  in
  ()
