open AState
open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils

let empty_lifting = { tmap = Map.empty (module Terms) }

let has_real_lifting p =
  let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
  not (Option.is_some (RType.unify_one ~verb:false tout (fst !_alpha)))

(**
  Return a projection function that can be symbolically evaluated.
*)
let proj_to_non_lifting p =
  if not (has_real_lifting p) then None
  else
    let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
    match (tout, fst !_alpha) with
    | TTup tl_lift, TTup tl' ->
        let args = List.map ~f:(fun t -> Variable.mk (Alpha.fresh ~s:"x" ()) ~t:(Some t)) tl_lift in
        let tuple_pattern = FPatTup (List.map ~f:(fun v -> FPatVar v) args) in
        let tup_orig = mk_tup (List.map ~f:mk_var (List.take args (List.length tl'))) in
        Some (mk_fun [ tuple_pattern ] tup_orig)
    | TTup tl_lift, _ ->
        let args = List.map ~f:(fun t -> Variable.mk (Alpha.fresh ~s:"x" ()) ~t:(Some t)) tl_lift in
        let tuple_pattern = FPatTup (List.map ~f:(fun v -> FPatVar v) args) in
        Some (mk_fun [ tuple_pattern ] (mk_var (List.hd_exn args)))
    | _ ->
        Log.error_msg "Ignoring type difference between ɑ and output of target.";
        None

(**
  Return a projection function that can be symbolically evaluated.
*)
let proj_to_lifting p =
  if not (has_real_lifting p) then None
  else
    let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
    match (tout, fst !_alpha) with
    | TTup tl_lift, TTup tl' ->
        let args = List.map ~f:(fun t -> Variable.mk (Alpha.fresh ~s:"x" ()) ~t:(Some t)) tl_lift in
        let tuple_pattern = FPatTup (List.map ~f:(fun v -> FPatVar v) args) in
        let tup_lifting =
          match List.map ~f:mk_var (List.drop args (List.length tl')) with
          | [ x ] -> x
          | _ as l -> mk_tup l
        in
        Some (mk_fun [ tuple_pattern ] tup_lifting)
    | TTup tl_lift, _ ->
        let args = List.map ~f:(fun t -> Variable.mk (Alpha.fresh ~s:"x" ()) ~t:(Some t)) tl_lift in
        let tuple_pattern = FPatTup (List.map ~f:(fun v -> FPatVar v) args) in
        let tup_lifting =
          match List.map ~f:mk_var (List.drop args 1) with [ x ] -> x | _ as l -> mk_tup l
        in
        Some (mk_fun [ tuple_pattern ] tup_lifting)
    | _ ->
        Log.error_msg "Ignoring type difference between ɑ and output of target.";
        None

(**
  Returns a symbolic function [f] such that [f original_part lifting_part]
  computes the tuple that corresponds to the lifted function from the original components
  of the function in [oringal_part] and the components of the lifting only in [lifting_part]
*)
let compose_parts p =
  if not (has_real_lifting p) then None
  else
    let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
    match (tout, fst !_alpha) with
    | TTup tl_lift, TTup tl' ->
        let args = List.map ~f:(fun t -> Variable.mk (Alpha.fresh ~s:"x" ()) ~t:(Some t)) tl_lift in
        let n = List.length tl' in
        let tuple_pattern_orig = FPatTup (List.map ~f:(fun v -> FPatVar v) (List.take args n)) in
        let tuple_pattern_lifting = FPatTup (List.map ~f:(fun v -> FPatVar v) (List.drop args n)) in
        let tup_all = mk_tup (List.map ~f:mk_var args) in
        Some (mk_fun [ tuple_pattern_orig; tuple_pattern_lifting ] tup_all)
    | TTup tl_lift, _ ->
        let args = List.map ~f:(fun t -> Variable.mk (Alpha.fresh ~s:"x" ()) ~t:(Some t)) tl_lift in
        let n = 1 in
        let pattern_orig = FPatVar (List.hd_exn args) in
        let tuple_pattern_lifting =
          match List.map ~f:(fun v -> FPatVar v) (List.drop args n) with
          | [ a ] -> a
          | _ as l -> FPatTup l
        in
        let tup_all = mk_tup (List.map ~f:mk_var args) in
        Some (mk_fun [ pattern_orig; tuple_pattern_lifting ] tup_all)
    | _ ->
        Log.error_msg "Ignoring type difference between ɑ and output of target.";
        None

let apply_lifting ~(p : psi_def) (l : RType.t list) : psi_def =
  (* Type inference on p.target to update types *)
  let target' =
    let new_out_type, _new_target_type =
      let open RType in
      match Variable.vtype_or_new p.psi_target.PMRS.pvar with
      | TFun (t_in, TTup old_ts) ->
          let tout = TTup (old_ts @ l) in
          (tout, RType.TFun (t_in, tout))
      | TFun (t_in, t_out) ->
          let tout = TTup (t_out :: l) in
          (TFun (t_in, tout), tout)
      | _ -> failwith "Type of target recursion skeleton should be a function."
    in
    let target' = PMRS.infer_pmrs_types (PMRS.clear_pmrs_types p.psi_target) in
    let free_theta = PMRS.extract_rec_input_typ target' in
    (* Update the input type, w.r.t to the the theta stored. *)
    PMRS.unify_one_with_update (free_theta, !_theta);
    (* Update the output type. *)
    (match RType.unify_one (Variable.vtype_or_new target'.pmain_symb) new_out_type with
    | Some subs -> Variable.update_var_types (RType.mkv subs)
    | None -> Log.error_msg "Failed to unify output types in lifting.");
    PMRS.infer_pmrs_types target'
  in
  Log.debug (fun ft () -> Fmt.(pf ft "@[After lifting:@;%a@]" (box PMRS.pp) target'));
  { p with psi_target = target'; psi_lifting = p.psi_lifting @ l }

let scalar ~(p : psi_def) (_l : refinement_loop_state) _synt_failure_info :
    (psi_def, solver_response) Result.t =
  Log.error_msg "Lifting is only a stub.";
  (*
    This function will perform the scalar lifting and call the loop continuation
    with the lifted problem.
  *)
  let p' = apply_lifting ~p [ RType.TInt ] in
  Ok p'
