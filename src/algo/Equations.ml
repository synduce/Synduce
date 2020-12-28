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
  Expand.replace_rhs_of_main p p.target (Reduce.reduce_pmrs p.target t)


let compute_lhs p t =
  Log.verbose (fun f () -> Fmt.(pf f "LHS t  = %a." pp_term t));
  let t' = Reduce.reduce_pmrs p.repr t in
  Log.verbose (fun f () -> Fmt.(pf f "LHS t' = %a." pp_term t'));
  let r_t = Expand.replace_rhs_of_main p p.repr t' in
  Log.verbose (fun f () -> Fmt.(pf f "LHS r_t = %a." pp_term r_t));
  let subst_params =
    let l = List.zip_exn p.orig.pargs p.target.pargs in
    List.map l ~f:(fun (v1, v2) -> mk_var v1, mk_var v2)
  in
  let f_r_t = Reduce.reduce_pmrs p.orig r_t in
  Log.verbose (fun f () -> Fmt.(pf f "LHS f_r_t = %a." pp_term f_r_t));
  let final = substitution subst_params f_r_t in
  Log.verbose (fun f () -> Fmt.(pf f "LHS final = %a." pp_term final));
  Expand.replace_rhs_of_main p p.orig final


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
  (if Set.length invariants > 0 then
     Log.debug Fmt.(fun frmt () -> pf frmt "Invariants:@[<hov 2>%a@]"
                       (list ~sep:comma pp_term) (Set.elements invariants))
   else Log.debug_msg "No invariants.");
  let pure_eqns =
    let f (t, lhs, rhs) =
      let applic x = substitution all_subs (Reduce.reduce_term (substitution all_subs x)) in
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


let mk_projs (tin : RType.t) (tl : RType.t list) (xi : Variable.t) =
  let f i t =
    Variable.mk ~t:(Some RType.(TFun(tin, t))) (xi.vname^(Int.to_string i))
  in List.mapi ~f tl


let revert_projs
    (orig_xi : VarSet.t)
    (projections : (int, variable list, Int.comparator_witness) Map.t)
    (soln : (string * (variable list) * term) list) :
  (string * variable list * term) list =
  (* Helper functions *)
  let find_soln s = 
    List.find_exn ~f:(fun (s', _, _) -> String.equal s.vname s') soln
  in 
  let join_bodies main_args first_body rest = 
    let f accum (_, args, body) =
      let subst =
        match List.zip args main_args with
        | Ok l -> List.map l ~f:(fun (v1,v2) -> mk_var v1, mk_var v2)
        | Unequal_lengths -> failwith "Projections should have same number of arguments."
      in
      accum @ [substitution subst body]
    in
    let tuple_elts =  List.fold ~f ~init:[first_body] rest in
    mk_tup tuple_elts
  in
  (* Helper sets *)
  let all_proj_names, xi_projected =
    let x0 = Map.to_alist projections in
    let x1 =
      List.concat (List.map ~f:(fun (id, l) -> List.map ~f:(fun e -> id, e) l) x0)
    in
    List.map ~f:(fun (_, v) -> v.vname) x1,
    VarSet.of_list (List.filter_map ~f:(fun (id, _) -> VarSet.find_by_id orig_xi id) x1)
  in
  let _, rest =
    let f (s, _, _) =
      List.mem all_proj_names ~equal:String.equal s
    in
    List.partition_tf ~f soln
  in
  (* Build for each xi projected *)
  let new_xi_solns =
    let f xi =
      let vls = Map.find_exn projections  xi.vid in
      let solns = List.map ~f:find_soln vls in
      match solns with
      | [] -> failwith "revert_projs : failed to find an expected solution."
      | [_, args, body] -> xi.vname, args, body (* This should not happen though. *)
      | (_, args1, body1) :: tl -> xi.vname, args1, join_bodies args1 body1 tl
    in
    List.map ~f (Set.elements xi_projected)
  in
  rest @ new_xi_solns


(* ============================================================================================= *)
(*                               SOLVING SYSTEMS OF EQUATIONS                                    *)
(* ============================================================================================= *)
let constraints_of_eqns
    (projections : (int, variable list, Int.comparator_witness) Map.t)
    (eqns : equation list)
  : command list =
  let apply_p = Analysis.apply_projections projections in
  let detupled_equations =
    let f (_, pre, lhs, rhs) =
      let lhs' = apply_p lhs and rhs' = apply_p rhs in
      let eqs = projection_eqns lhs' rhs' in
      List.map ~f:(fun (_l, _r) -> pre, _l, _r) eqs
    in
    List.concat (List.map ~f eqns)
  in
  let eqn_to_constraint (pre, lhs, rhs) =
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
  List.map ~f:eqn_to_constraint detupled_equations


let synthfuns_of_unknowns ?(bools = false) ?(ops = OpSet.empty) (unknowns : VarSet.t) =
  (*  Flatten the inputs (tuples) -> scalars *)
  let unknowns_projs =
    let f xi =
      match Variable.vtype_or_new xi with
      | RType.TFun(tin, TTup tl) -> xi, Some (mk_projs tin tl xi)
      | _ -> xi, None
    in
    List.map ~f (Set.elements unknowns)
  in
  let xi_formals (xi : variable) : sorted_var list * sygus_sort =
    match Variable.vtype_or_new xi with
    | RType.TFun(TTup(targs), tres) -> sorted_vars_of_types targs, sort_of_rtype tres
    | RType.TFun(targ, tres) -> sorted_vars_of_types [targ], sort_of_rtype tres
    | t -> [], sort_of_rtype t
  in
  let f (xi, xi_projs) =
    let f xi =
      let args, ret_sort = xi_formals xi in
      let grammar = Grammars.generate_grammar ~bools ops args ret_sort in
      CSynthFun (xi.vname, args, ret_sort, grammar)
    in
    match xi_projs with
    | None -> [f xi]
    | Some projs -> List.map ~f projs
  in
  let proj_map =
    let mmap =
      Map.of_alist (module Int)
        (List.map ~f:(fun (x, p) -> x.vid, p)
           (* Only select relevant xi for projection *)
           (List.filter_map
              ~f:(fun (_x, _o) -> match _o with Some o -> Some (_x, o) | None -> None)
              unknowns_projs))
    in
    match mmap with 
    | `Ok x -> x
    | `Duplicate_key _ -> failwith "Unexpected error while projecting."
  in
  List.concat (List.map ~f unknowns_projs), proj_map


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
  let synth_objs, projections =
    synthfuns_of_unknowns ~bools:has_ite ~ops:all_operators p.target.pparams
  in
  let var_decls = List.map ~f:declaration_of_var (Set.elements free_vars) in
  let constraints = constraints_of_eqns projections eqns in
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
      let soln0 = List.map ~f:parse_synth_fun resps in
      let soln =
        if Map.length projections > 0 then
          revert_projs p.target.pparams projections soln0
        else soln0
      in
      Utils.Log.debug_msg
        Fmt.(str "@[<hov 2>Solution found: @;%a"
               (list ~sep:comma (fun fmrt (s, args, bod) ->
                    pf fmrt "@[<hov 2>@[%s(%a)@] = @[%a@]@]"
                      s (list ~sep:comma Variable.pp) args pp_term bod)) soln);

      resp, Some soln

    | RInfeasible -> RInfeasible, None

    | RFail -> RFail, None

    | RUnknown -> RUnknown, None
  in
  match Syguslib.Solvers.CVC4.solve_commands commands with
  | Some resp -> handle_response resp
  | None -> RFail, None

