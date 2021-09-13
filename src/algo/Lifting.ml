open AState
open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils

let msg_lifting () =
  Log.info
    Fmt.(
      fun frmt () ->
        (styled (`Fg `Black) (styled (`Bg (`Hi `Red)) (fun frmt () -> pf frmt "Lifting ... ")))
          frmt ())

module LiftingMap = struct
  type t = ((int * term) * term) list

  let empty : t = []

  let key_match ~key:(i1, t1) i2 t2 = if i1 = i2 then Analysis.matches ~pattern:t1 t2 else None

  let get (map : t) ((i, t0) : int * term) =
    List.find_map map ~f:(fun ((i1, t1), tr) ->
        match key_match ~key:(i1, t1) i t0 with
        | Some sub -> Some (substitution (VarMap.to_subst sub) tr)
        | None -> None)

  let is_empty (a : t) = List.is_empty a

  let set (map : t) ((i, t0) : int * term) (tr : term) = ((i, t0), tr) :: map
end

let empty_lifting = { tmap = LiftingMap.empty }

let is_empty_lifting lifting = LiftingMap.is_empty lifting.tmap

let alpha_component_count () = match !_alpha with RType.TTup tl -> List.length tl | _ -> 1

let decompose_t (p : psi_def) (t : term) : (int * term) option =
  let g = p.psi_target.pmain_symb in
  match t.tkind with
  | TSel (t', i) ->
      let n = alpha_component_count () in
      if i >= n then
        match t'.tkind with
        | TApp ({ tkind = TVar f; _ }, [ targ ]) when Variable.(equal f g) -> Some (i, targ)
        | _ -> None
      else None
  | _ -> None

let recompose_t (p : psi_def) (t : term) (i : int) : term =
  let g = p.psi_target.pmain_symb in
  mk_sel (mk_app (mk_var g) [ t ]) i

(**
  Find the expression of the lifting for an input term.
*)
let get_mapped_value ~(p : psi_def) (l : lifting) (t : term) =
  Option.(decompose_t p t >>= LiftingMap.get l.tmap)

(**
  Interactively add an expression for the lifting.
*)
let interactive_add_lifting_expression ~p (l : lifting) (a : term) (i : int) : lifting * term option
    =
  let env = VarSet.to_env (Analysis.free_variables a) in
  let g = p.psi_target.pmain_symb in
  (* Prompt the user to add lemmas. *)
  Log.info (fun frmt () ->
      Fmt.(pf frmt "Please provide a value for (%s %a).%i:" g.vname pp_term a i));
  match Stdio.In_channel.input_line Stdio.stdin with
  | None | Some "" ->
      Log.info (fun frmt () -> Fmt.pf frmt "No lifting provided.");
      (l, None)
  | Some x -> (
      let smtterm =
        try
          let sexpr = Sexplib.Sexp.of_string x in
          Smtlib.SmtLib.smtTerm_of_sexp sexpr
        with Failure _ -> None
      in
      let l_term = Option.map ~f:(SmtInterface.term_of_smt env) smtterm in
      match l_term with
      | None -> (l, None)
      | Some x -> (
          match LiftingMap.get l.tmap (i, a) with
          | Some _ -> (l, Some x)
          | None -> ({ tmap = LiftingMap.set l.tmap (i, a) x }, Some x)))

let replace_boxed_expressions ~(p : psi_def) (l : lifting) =
  let case _ t = match t.tkind with TBox t' -> get_mapped_value ~p l t' | _ -> None in
  transform ~case

(** A problem definition is lifted if the ouput type of the target recursion skeleton
  is different from the output type of the reference function (which is unchanged
  and stored in AState._alpha)
*)
let is_lifted (p : psi_def) : bool =
  let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
  not (Result.is_ok (RType.unify_one tout !_alpha))

let lift_count (p : psi_def) : int =
  let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
  match tout with RType.TTup tl -> List.length tl - alpha_component_count () | _ -> 0

(* ============================================================================================= *)
(*                       PROJECTIONS TO AND FROM LIFTING                                         *)
(* ============================================================================================= *)

(**
  Return a projection function that can be symbolically evaluated.
*)
let proj_to_non_lifting (p : psi_def) : term option =
  if not (is_lifted p) then None
  else
    let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
    match (tout, !_alpha) with
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

let is_proj_function (p : psi_def) (t : term) : bool =
  match t.tkind with
  | TFun ([ tuple_arg_pattern ], tuple_body) -> (
      match tuple_arg_pattern with
      | FPatTup t ->
          if List.length t = lift_count p + alpha_component_count () then
            match tuple_body.tkind with
            | TTup tl -> List.length tl = alpha_component_count ()
            | _ -> alpha_component_count () = 1
          else false
      | _ -> false)
  | _ -> false

(**
    Return function f such that f(a) = a.i, .. a.i + n if i, .. i+n where
    i, ..., i+n are the components of the lifting.
*)
let proj_to_lifting (p : psi_def) : (term -> term) option =
  if not (is_lifted p) then None
  else
    let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
    match (tout, !_alpha) with
    | TTup tl_lift, TTup tl' ->
        let n = List.length tl' in
        Some
          (fun a ->
            match
              List.mapi ~f:(fun i t -> mk_sel ~typ:(Some t) a (n + i)) (List.drop tl_lift n)
            with
            | [] -> mk_tup []
            | [ x ] -> x
            | hd :: tl -> mk_tup (hd :: tl))
    | TTup tl_lift, _ ->
        Some
          (fun a ->
            match
              List.mapi ~f:(fun i t -> mk_sel ~typ:(Some t) a (1 + i)) (List.drop tl_lift 1)
            with
            | [] -> mk_tup []
            | [ x ] -> x
            | hd :: tl -> mk_tup (hd :: tl))
    | _ ->
        Log.error_msg "Ignoring type difference between ɑ and output of target.";
        None

(**
  Returns a symbolic function [f] such that [f original_part lifting_part]
  computes the tuple that corresponds to the lifted function from the original components
  of the function in [oringal_part] and the components of the lifting only in [lifting_part]
*)
let compose_parts (p : psi_def) : term option =
  if not (is_lifted p) then None
  else
    let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
    match (tout, !_alpha) with
    | TTup tl_lift, TTup tl' ->
        let args = List.map ~f:(fun t -> Variable.mk (Alpha.fresh ~s:"x" ()) ~t:(Some t)) tl_lift in
        let n = List.length tl' in
        let tuple_pattern_orig = FPatTup (List.map ~f:(fun v -> FPatVar v) (List.take args n)) in
        let tuple_pattern_lifting =
          match List.drop args n with
          | [ x ] -> FPatVar x
          | _ :: _ as l -> FPatTup (List.map ~f:(fun v -> FPatVar v) l)
          | _ -> failwith "Impossible."
        in
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

let ith_type (p : psi_def) (i : int) : RType.t option =
  let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar) in
  match tout with RType.TTup tl -> List.nth tl i | _ -> if i = 0 then Some tout else None

(* ============================================================================================= *)
(*                      LIFTING FUNCTIONS                                                        *)
(* ============================================================================================= *)

(**
  [apply_lifting ~p l] lifts [p.psi_target] by extending the output with [l].
*)
let apply_lifting ~(p : psi_def) (l : RType.t list) : psi_def =
  (* Type inference on p.target to update types *)
  let target' =
    let new_out_type =
      let open RType in
      match snd (fun_typ_unpack (Variable.vtype_or_new p.psi_target.PMRS.pvar)) with
      | TTup old_ts -> TTup (old_ts @ l)
      | t_out -> TTup (t_out :: l)
    in
    let target' = PMRS.infer_pmrs_types (PMRS.clear_pmrs_types p.psi_target) in
    let free_theta = PMRS.extract_rec_input_typ target' in
    (* Update the input type, w.r.t to the the theta stored. *)
    PMRS.unify_one_with_update (free_theta, !_theta);
    (* Update the output type. *)
    (match
       RType.unify_one
         (snd (RType.fun_typ_unpack (Variable.vtype_or_new target'.pmain_symb)))
         new_out_type
     with
    | Ok subs -> Variable.update_var_types (RType.mkv subs)
    | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        Log.error_msg "Failed to unify output types in lifting.");
    PMRS.infer_pmrs_types target'
  in
  (* ⚠️ !TODO! : updates all the "ensures" *)
  Log.debug (fun ft () -> Fmt.(pf ft "@[After lifting:@;%a@]" (box PMRS.pp) target'));
  { p with psi_target = target'; psi_lifting = p.psi_lifting @ l }

let deduce_lifting_expressions ~p (lif : lifting) (lemma : term option) (lhs : term) (rhs : term) :
    lifting =
  let boxes =
    reduce ~init:[]
      ~case:(fun _ t -> match decompose_t p t with Some (i, a) -> Some [ (i, a) ] | _ -> None)
      ~join:( @ ) rhs
  in
  if !Config.interactive_lifting then
    List.fold ~init:lif
      ~f:(fun l (i, t) ->
        match LiftingMap.get l.tmap (i, t) with
        | Some _ -> l
        | None -> fst (interactive_add_lifting_expression ~p l t i))
      boxes
  else
    let subs, var_to_linput =
      List.unzip
        (List.map boxes ~f:(fun (i, t) ->
             match LiftingMap.get lif.tmap (i, t) with
             | Some e -> ((recompose_t p t i, e), None)
             | None ->
                 let typ_i = Option.value_exn (ith_type p i) in
                 let x = Variable.mk (Alpha.fresh ~s:(Fmt.str "_L_%i" i) ()) ~t:(Some typ_i) in
                 ((recompose_t p t i, mk_var x), Some (x, (i, t)))))
    in
    let var_to_linput = List.filter_opt var_to_linput in
    let rec as_unknown_app t =
      match t.tkind with
      | TApp (f, [ arg ]) when is_proj_function p f -> as_unknown_app arg
      | TApp ({ tkind = TVar f; _ }, args) ->
          if Set.mem p.psi_target.psyntobjs f then Some args else None
      | _ -> None
    in
    let rhs' = substitution subs rhs in
    match as_unknown_app rhs' with
    | Some rhs_args ->
        let var_to_lexpr =
          Deduction.Solver.functional_equation ~func_side:rhs_args ~lemma lhs
            (List.map ~f:(fun (x, (_, t)) -> (x, Analysis.free_variables t)) var_to_linput)
        in
        List.fold var_to_lexpr ~init:lif ~f:(fun l (v, t) ->
            let i, t0 = List.Assoc.find_exn ~equal:Variable.equal var_to_linput v in
            { tmap = LiftingMap.set l.tmap (i, t0) t })
    | None -> lif

(* ============================================================================================= *)
(*                       MAIN ENTRY POINT                                                        *)
(* ============================================================================================= *)

(**
  Perform a scalar lifting.
*)
let scalar ~(p : psi_def) (l : refinement_loop_state) _synt_failure_info :
    (psi_def * refinement_loop_state, solver_response) Result.t =
  (*
    This function will perform the scalar lifting and continue the refinement loop
    with the lifted problem.
  *)
  let p' = apply_lifting ~p [ RType.TInt ] in
  Ok (p', l)
