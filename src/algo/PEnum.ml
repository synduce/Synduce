open AState
open Base
open Lang
open Term
open PMRS
open Utils

type rloc =
  { rid : int
  ; rrule : rewrite_rule
  ; rxi : variable
  ; rtokens : VarSet.t
  }

(* ============================================================================================= *)
(*                    ANALYSIS FUNCTIONS                                                         *)
(* ============================================================================================= *)

let analyze_rec_args_at_loc (p : psi_def) (r : rloc) =
  match Map.find p.psi_target.prules r.rid with
  | Some (_, lhs_args, _, _) ->
    Set.partition_tf r.rtokens ~f:(fun v ->
        (not (RType.is_base (Variable.vtype_or_new v)))
        && List.mem lhs_args ~equal:Variable.equal v)
  | None -> VarSet.empty, VarSet.empty
;;

(* ============================================================================================= *)
(*                    RECURSION SKELETON EXPANSION FUNCTIONS                                     *)
(* ============================================================================================= *)

let extend_function ~from:(f : variable) ~to_:(g : variable) (extra_args : term list) =
  let case _ t =
    match t.tkind with
    | TApp ({ tkind = TVar f'; _ }, args) when Variable.equal f f' ->
      Some (mk_app (mk_var g) (args @ extra_args))
    | TVar f' when Variable.equal f f' -> Some (mk_app (mk_var g) extra_args)
    | _ -> None
  in
  transform ~case
;;

let mk_with_deconstruction
    ~(p : psi_def)
    (r : rloc)
    ((dec, _no_dec) : VarSet.t * VarSet.t)
  =
  let deconstruct_one (nt, lhs_args, lhs_pat, rhs) v =
    match RType.get_variants (Variable.vtype_or_new v) with
    | _ :: _ as cases ->
      let f (constrname, constrargs) =
        Fmt.(
          pf
            stdout
            "---> Construct %s(%a) -> %a@."
            constrname
            (list ~sep:comma RType.pp)
            constrargs
            pp_term
            rhs);
        nt, lhs_args, lhs_pat, rhs
      in
      let new_rules =
        match List.map ~f cases with
        | [ a ] -> Some (Map.set p.psi_target.prules ~key:r.rid ~data:a)
        | hd :: tl ->
          let map0 = Map.set p.psi_target.prules ~key:r.rid ~data:hd in
          (* Map cannot be empty here. *)
          let max_rule_id, _ = Map.max_elt_exn map0 in
          let new_rules =
            List.fold_left
              tl
              ~init:(map0, max_rule_id + 1)
              ~f:(fun (rules, mrid) new_rule ->
                Map.add_exn rules ~key:mrid ~data:new_rule, mrid + 1)
          in
          Some (fst new_rules)
        | _ -> None
      in
      (match new_rules with
      | Some nr -> [ { p with psi_target = { p.psi_target with prules = nr } } ]
      | None -> [])
    | _ -> []
    (* No variants, nothing to be done here. *)
  in
  match Map.find p.psi_target.prules r.rid with
  | Some rrule -> List.concat_map (Set.elements dec) ~f:(deconstruct_one rrule)
  | None -> []
;;

let mk_with_constant_args ~(p : psi_def) (r : rloc) cargs =
  let new_xi =
    let in_t, re_t = RType.fun_typ_unpack (Variable.vtype_or_new r.rxi) in
    (* Input type is extended by adding arguments *)
    let t =
      Some (RType.fun_typ_pack (in_t @ List.map ~f:Variable.vtype_or_new cargs) re_t)
    in
    Variable.mk ~t (Alpha.fresh ~s:r.rxi.vname ())
  in
  let add_arg_and_replace_unknown _t =
    let nt, lhs, pat, rhs = r.rrule in
    nt, lhs, pat, extend_function ~from:r.rxi ~to_:new_xi (List.map ~f:mk_var cargs) rhs
  in
  let unknowns =
    let unknown_in_other_rules =
      Map.existsi p.psi_target.prules ~f:(fun ~key ~data ->
          let _, _, _, rhs = data in
          (not (key = r.rid)) && Set.mem (Analysis.free_variables rhs) r.rxi)
    in
    if unknown_in_other_rules
    then p.psi_target.psyntobjs
    else Set.remove p.psi_target.psyntobjs r.rxi
  in
  let new_psi_target =
    { p.psi_target with
      psyntobjs = Set.add unknowns new_xi
    ; prules = Map.update p.psi_target.prules r.rid ~f:add_arg_and_replace_unknown
    }
  in
  ( [ { p with psi_id = new_psi_id (); psi_target = new_psi_target } ]
  , { r with rxi = new_xi } )
;;

let mk_with_rec_args ~(p : psi_def) (r : rloc) =
  if Set.is_empty r.rtokens
  then []
  else (
    (* Analayse whether the arguments are usable as-is or need to be unpacked. *)
    let args_require_deconstruction, args_usable = analyze_rec_args_at_loc p r in
    (if not (Set.is_empty args_usable) then [] else [])
    @
    if not (Set.is_empty args_require_deconstruction)
    then mk_with_deconstruction ~p r (args_require_deconstruction, args_usable)
    else [])
;;

let mk_new_problem ~(p : psi_def) (r : rloc) : psi_def list =
  (* First extend the unknowns with constant argsr. *)
  let with_extra_constant_args, updated_objective =
    let cargs, rargs =
      let of_base_type, of_rec_type =
        Set.partition_tf r.rtokens ~f:(fun v -> RType.is_base (Variable.vtype_or_new v))
      in
      Set.elements of_base_type, of_rec_type
    in
    if List.length cargs > 0
    then (
      let new_p, new_r = mk_with_constant_args ~p r cargs in
      new_p, { new_r with rtokens = rargs })
    else [], r
  in
  with_extra_constant_args
  @
  match with_extra_constant_args with
  | _ :: _ ->
    mk_with_rec_args ~p:(List.last_exn with_extra_constant_args) updated_objective
  | [] -> mk_with_rec_args ~p r
;;

let mk_extra_toplevel_problems (p : psi_def) =
  let g = p.psi_target in
  let paramset = VarSet.of_list g.pargs in
  (* Check for each rule.  *)
  let f ((i, rrule) : int * rewrite_rule) =
    let _, lhs_args, lhs_pat, rhs = rrule in
    let rhs_argset = Analysis.free_variables ~include_functions:false rhs in
    let lhs_argset =
      Set.union
        (VarSet.of_list lhs_args)
        (Option.value_map lhs_pat ~default:VarSet.empty ~f:(fun p ->
             Analysis.free_variables (Term.term_of_pattern p)))
    in
    if not (Set.are_disjoint g.psyntobjs rhs_argset)
    then (
      let more_args = Set.diff (Set.union paramset lhs_argset) rhs_argset in
      if not (Set.is_empty more_args)
      then (
        (* TODO: come up with a better strategy to find extensions.
        Should new unknowns be a 1-to-1 map with original unknowns, or should we allow splitting?
        *)
        let unknowns = Set.inter p.psi_target.psyntobjs (Analysis.free_variables rhs) in
        List.map
          ~f:(fun rxi -> { rid = i; rrule; rxi; rtokens = more_args })
          (Set.elements unknowns))
      else [])
    else []
  in
  let subspecs = List.concat_map ~f (Map.to_alist g.prules) in
  p :: List.concat_map ~f:(mk_new_problem ~p) subspecs
;;

(* ============================================================================================= *)
(*                    MAIN ENTRY POINT                                                           *)
(* ============================================================================================= *)

let enumerate_p (p : psi_def) =
  let res = mk_extra_toplevel_problems p in
  Log.info Fmt.(fun fmt () -> pf fmt "%i subproblems." (List.length res));
  res
;;
