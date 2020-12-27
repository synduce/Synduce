open Base
open Lang
open Lang.Term
open AState
open Syguslib.Sygus
open SygusInterface
open Utils
module SmtI = SmtInterface


type equation = term * term option * term * term

let pp_equation (f : Formatter.t) (orig, inv, lhs, rhs : equation) =
  match inv with
  | Some inv -> Fmt.(pf f "@[<hov 2>{%a}@[%a =>@;%a = %a@]@]" pp_term orig pp_term inv pp_term lhs pp_term rhs)
  | None -> Fmt.(pf f "@[<hov 2>{%a}@;@[%a = %a@]@]" pp_term orig pp_term lhs pp_term rhs)


let check_equation ~(p : psi_def) (_, pre, lhs, rhs : equation) : bool =
  (match Expand.nonreduced_terms_all p lhs, Expand.nonreduced_terms_all p rhs with
   | [], [] -> true
   | _ -> false) &&
  (match pre with
   | None -> true
   | Some t -> match Expand.nonreduced_terms_all p t with | [] -> true | _ -> false)

let compute_rhs p t =
  Fmt.()
    (Expand.replace_rhs_of_main p p.target (Reduce.reduce_pmrs p.target t))

let compute_lhs p t =
  let r_t = Expand.replace_rhs_of_main p p.repr (Reduce.reduce_pmrs p.repr t) in
  let subst_params =
    let l = List.zip_exn p.orig.pargs p.target.pargs in
    List.map l ~f:(fun (v1, v2) -> mk_var v1, mk_var v2)
  in
  substitution subst_params
    (Expand.replace_rhs_of_main p p.orig (Reduce.reduce_pmrs p.orig r_t))


let projection_eqns (lhs : term) (rhs : term) =
  match lhs.tkind, rhs.tkind with
  | TTup lhs_tl, TTup rhs_tl -> List.map ~f:(fun (r,l) -> r,l) (List.zip_exn lhs_tl rhs_tl)
  | _ -> [lhs, rhs]

let invar invariants lhs_e rhs_e =
  let f inv_expr =
    not (Set.is_empty
           (Set.inter
              (Set.union (Analysis.free_variables lhs_e) (Analysis.free_variables rhs_e))
              (Analysis.free_variables inv_expr)))
  in
  let conjs = List.filter ~f (Set.elements invariants) in
  mk_assoc Binop.And conjs


let make ~(p : psi_def) (tset : TermSet.t) : equation list =
  let eqns =
    let fold_f eqns t =
      let lhs = compute_lhs p t in
      let rhs = compute_rhs p t in
      eqns @ [t, lhs, rhs]
    in
    Set.fold ~init:[] ~f:fold_f tset
  in
  let all_subs, invariants =
    Expand.subst_recursive_calls p
      (List.concat (List.map ~f:(fun (_, lhs, rhs) -> [lhs; rhs]) eqns))
  in
  Log.debug Fmt.(fun frmt () -> pf frmt "Invariants:@[<hov 2>%a@]@."
                    (list ~sep:comma pp_term) (Set.elements invariants));
  let pure_eqns =
    let f (t, lhs, rhs) =
      let applic x = Reduce.reduce_term (substitution all_subs x) in
      let lhs' = applic lhs and rhs' = applic rhs in
      let projs = projection_eqns lhs' rhs' in
      List.map ~f:(fun (lhs,rhs) -> t, invar invariants lhs rhs, lhs, rhs) projs
    in
    List.concat (List.map ~f eqns)
  in
  Log.verbose_msg Fmt.(str "Equations > make@;@[%a@]" (list ~sep:sp pp_equation) pure_eqns);
  match List.find ~f:(fun eq -> not (check_equation ~p eq)) pure_eqns with
  | Some not_pure ->
    Log.error_msg Fmt.(str "Not pure: %a" pp_equation not_pure);
    failwith "Equation not pure."
  | None ->  pure_eqns


(* ============================================================================================= *)
(*                               SOLVING SYSTEMS OF EQUATIONS                                    *)
(* ============================================================================================= *)
let constraints_of_eqns (eqns : equation list) : command list =
  let eqn_to_constraint (_, pre, lhs, rhs) =
    match pre with
    | Some precondition ->
      CConstraint (
        SyApp(IdSimple "or",
              [
                SyApp(IdSimple "not", [sygus_of_term precondition]);
                SyApp(IdSimple "=", [sygus_of_term lhs; sygus_of_term rhs])
              ]))
    | None -> CConstraint (SyApp(IdSimple "=", [sygus_of_term lhs; sygus_of_term rhs]))
  in
  List.map ~f:eqn_to_constraint eqns


(* let mk_projs (tin : RType.t) (tl : RType.t list) (xi : Variable.t) =
   let f t =
    Variable.mk
   in
   in List.map ~f tl *)


let synthfuns_of_unknowns ?(bools = false) ?(ops = OpSet.empty) (unknowns : VarSet.t) : command list =
  (*  Flatten the inputs (tuples) -> scalars *)
  (* let proj_unknowns =
     let f xi =
      match xi.ttyp with
      | TFun(tin, TTup tl) -> xi, mk_projs tin tl xi
      | _ -> xi, xi
     in
     List.map ~f (Set.elements unknowns)
     in *)
  (*  Decompose into multiple functions for tuples. *)
  let decompose_xi_args (xi : variable) =
    match Variable.vtype_or_new xi with
    | RType.TFun(TTup(targs), tres) -> sorted_vars_of_types targs, sort_of_rtype tres
    | RType.TFun(targ, tres) -> sorted_vars_of_types [targ], sort_of_rtype tres
    | t -> [], sort_of_rtype t
  in
  let f xi =
    let args, ret_sort = decompose_xi_args xi in
    let grammar = Grammars.generate_grammar ~bools ops args ret_sort in
    CSynthFun (xi.vname, args, ret_sort, grammar)
  in
  List.map ~f (Set.elements unknowns)


(* let pre_solve ~(p : psi_def) (eqns : equation list) = *)


let solve ~(p : psi_def) (eqns : equation list) =
  let free_vars, all_operators, has_ite =
    let f (fvs, ops, hi) (_, _, lhs, rhs) =
      VarSet.union_list [fvs; Analysis.free_variables lhs; Analysis.free_variables rhs],
      Set.union ops (Set.union (Grammars.operators_of lhs) (Grammars.operators_of rhs)),
      hi || (Analysis.has_ite lhs) || (Analysis.has_ite rhs)
    in
    let fvs, ops, hi = List.fold eqns ~f ~init:(VarSet.empty, Set.empty (module Operator), false) in
    Set.diff fvs p.target.pparams, ops, hi
  in
  (* Commands *)
  let set_logic = CSetLogic(Grammars.logic_of_operator all_operators) in
  let synth_objs = synthfuns_of_unknowns ~bools:has_ite ~ops:all_operators p.target.pparams in
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
    | RSuccess (resps) ->
      let soln = List.map ~f:parse_synth_fun resps in
      Utils.Log.debug_msg
        Fmt.(str "@[<hov 2>Solution found: @;%a"
               (list (fun fmrt (s, args, bod) ->
                    pf fmrt "@[<hov 2>@[%s(%a)@] = @[%a@]@]" s (list ~sep:comma Variable.pp) args pp_term bod)) soln);
      resp, Some  soln
    | RInfeasible -> RInfeasible, None
    | RFail -> RFail, None
    | RUnknown -> RUnknown, None
  in
  match Syguslib.Solvers.CVC4.solve_commands commands with
  | Some resp -> handle_response resp
  | None -> RFail, None

