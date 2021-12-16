open AState
open Base
open Lang
open Term
open PMRS
open Utils

type rloc =
  { rid : int
  ; rxi : variable
  ; rtokens : VarSet.t
  }

let _pp_rloc (frmt : Formatter.t) (r : rloc) =
  Fmt.(pf frmt "@[[%i:%a]<%a>@]" r.rid Variable.pp r.rxi VarSet.pp r.rtokens)
;;

(* ============================================================================================= *)
(*                    ANALYSIS FUNCTIONS                                                         *)
(* ============================================================================================= *)

let analyze_rec_args_at_loc (p : psi_def) (r : rloc) =
  match Map.find p.psi_target.prules r.rid with
  | Some (_, lhs_args, _, _) ->
    Set.partition_tf r.rtokens ~f:(fun v ->
        (not (RType.is_base (Variable.vtype_or_new v)))
        (* Only the last element can be deconstructed in the PMRS model. *)
        && List.is_prefix lhs_args ~equal:Variable.equal ~prefix:[ v ])
  | None -> VarSet.empty, VarSet.empty
;;

let safe_remove_unknown (r : rloc) p =
  let unknown_in_other_rules =
    Map.existsi p.psi_target.prules ~f:(fun ~key ~data ->
        let _, _, _, rhs = data in
        (not (key = r.rid)) && Set.mem (Analysis.free_variables rhs) r.rxi)
  in
  if unknown_in_other_rules
  then p.psi_target.psyntobjs
  else Set.remove p.psi_target.psyntobjs r.rxi
;;

let applicable_nonterminals (p : psi_def) (args : TermSet.t) =
  let f nont =
    let in_typs, _ = RType.fun_typ_unpack (Variable.vtype_or_new nont) in
    let arg_choices =
      List.fold_left
        in_typs
        ~init:[ [], args ]
        ~f:(fun args_so_far in_ty ->
          List.concat_map args_so_far ~f:(fun (ts, rem_args) ->
              List.map
                ~f:(fun v -> ts @ [ v ], Set.remove rem_args v)
                (Set.elements (TermSet.filter_by_type rem_args in_ty))))
    in
    List.map ~f:(fun (argvs, _) -> nont, argvs) arg_choices
  in
  List.concat_map ~f (Set.elements p.psi_target.pnon_terminals)
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

let new_recursive_cases p xi (nt, args, decons_arg, rhs) (constrname, constrargs) =
  let pat_vars =
    List.map
      ~f:(fun typ -> Variable.mk ~t:(Some typ) (Alpha.fresh ~s:decons_arg.vname ()))
      constrargs
  in
  let new_rec_calls =
    let possible_rec_calls =
      applicable_nonterminals
        p
        (TermSet.of_varset (Set.union (VarSet.of_list args) (VarSet.of_list pat_vars)))
    in
    List.map ~f:(fun (nont, arg) -> mk_app (mk_var nont) arg) possible_rec_calls
  in
  let new_scalars =
    List.filter ~f:(fun v -> RType.is_base (Variable.vtype_or_new v)) pat_vars
  in
  let f extra_args =
    let pat = PatConstr (constrname, List.map ~f:mk_pat_var pat_vars) in
    let new_args = List.map ~f:mk_var new_scalars @ extra_args in
    let new_unknown =
      (* Input type is extended by adding arguments *)
      let in_t, out_t = RType.fun_typ_unpack (Variable.vtype_or_new xi) in
      let extra_t = List.map ~f:type_of new_args in
      let t = Some (RType.fun_typ_pack (in_t @ extra_t) out_t) in
      Variable.mk ~t (Alpha.fresh ~s:xi.vname ())
    in
    let new_rhs = extend_function ~from:xi ~to_:new_unknown new_args rhs in
    let new_rewrite_rule = nt, args, Some pat, new_rhs in
    new_unknown, new_rewrite_rule
  in
  match new_rec_calls with
  | [] -> [ f [] ]
  | _ -> List.map ~f (subsets new_rec_calls)
;;

let mk_with_deconstruction
    ~(p : psi_def)
    (r : rloc)
    ((dec, _no_dec) : VarSet.t * VarSet.t)
  =
  let deconstruct_one (nt, lhs_args, _, rhs) v =
    match RType.get_variants (Variable.vtype_or_new v) with
    | _ :: _ as cases ->
      let last_arg = List.last_exn lhs_args in
      let other_args = List.drop_last_exn lhs_args in
      let new_rulesets =
        cartesian_nary_product
          (List.map
             ~f:(new_recursive_cases p r.rxi (nt, other_args, last_arg, rhs))
             cases)
      in
      let f new_cases =
        let new_rules =
          match new_cases with
          | [ (a, b) ] ->
            Some (VarSet.singleton a, Map.set p.psi_target.prules ~key:r.rid ~data:b)
          | (new_unknown, new_rewrite_rule) :: tl ->
            let map0 = Map.set p.psi_target.prules ~key:r.rid ~data:new_rewrite_rule in
            (* Map cannot be empty here. *)
            let max_rule_id, _ = Map.max_elt_exn map0 in
            let new_unknowns, new_rules, _ =
              List.fold_left
                tl
                ~init:(VarSet.singleton new_unknown, map0, max_rule_id + 1)
                ~f:(fun (xis, rules, mrid) (new_unknown, new_rule) ->
                  ( Set.add xis new_unknown
                  , Map.add_exn rules ~key:mrid ~data:new_rule
                  , mrid + 1 ))
            in
            Some (new_unknowns, new_rules)
          | _ -> None
        in
        match new_rules with
        | Some (n_xis, nr) ->
          let psyntobjs = safe_remove_unknown r p in
          [ { p with
              psi_id = new_psi_id ()
            ; psi_target =
                { p.psi_target with psyntobjs = Set.union n_xis psyntobjs; prules = nr }
            }
          ]
        | None -> []
      in
      List.concat_map ~f new_rulesets
    | _ -> []
    (* No variants, nothing to be done here. *)
  in
  match Map.find p.psi_target.prules r.rid with
  | Some rrule -> List.concat_map (Set.elements dec) ~f:(deconstruct_one rrule)
  | None -> []
;;

let mk_with_constant_args ~(p : psi_def) (r : rloc) (cargs : term list) =
  let new_xi =
    let in_t, re_t = RType.fun_typ_unpack (Variable.vtype_or_new r.rxi) in
    (* Input type is extended by adding arguments *)
    let t = Some (RType.fun_typ_pack (in_t @ List.map ~f:type_of cargs) re_t) in
    Variable.mk ~t (Alpha.fresh ~s:r.rxi.vname ())
  in
  let add_arg_and_replace_unknown _t =
    let nt, lhs, pat, rhs = Map.find_exn p.psi_target.prules r.rid in
    nt, lhs, pat, extend_function ~from:r.rxi ~to_:new_xi cargs rhs
  in
  let new_psi_target =
    { p.psi_target with
      psyntobjs = Set.add (safe_remove_unknown r p) new_xi
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
    (if not (Set.is_empty args_usable)
    then (
      let arg_choices =
        List.map
          ~f:(fun (f, args) -> mk_app (mk_var f) args)
          (applicable_nonterminals p (TermSet.of_varset args_usable))
      in
      let new_ps =
        List.concat_map
          ~f:(fun arg_choices -> fst (mk_with_constant_args ~p r arg_choices))
          (subsets arg_choices)
      in
      new_ps)
    else [])
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
      let new_p, new_r = mk_with_constant_args ~p r (List.map ~f:mk_var cargs) in
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
        let unknowns = Set.inter p.psi_target.psyntobjs (Analysis.free_variables rhs) in
        List.concat_map
          ~f:(fun rxi ->
            let _, _, _, rhs_rrule = rrule in
            let more_args = Set.diff more_args (Analysis.argset_of rxi rhs_rrule) in
            if Set.is_empty more_args
            then []
            else [ { rid = i; rxi; rtokens = more_args } ])
          (Set.elements unknowns))
      else [])
    else []
  in
  let subspecs = List.concat_map ~f (Map.to_alist g.prules) in
  let new_ps =
    if List.length subspecs > 0
    then
      List.fold_left subspecs ~init:[ p ] ~f:(fun accum subspec ->
          Log.verbose
            Fmt.(
              fun fmt () ->
                pf
                  fmt
                  "@.༆ Apply %a to:@.%a@."
                  _pp_rloc
                  subspec
                  (list (PMRS.pp ~short:false))
                  (List.map ~f:(fun p' -> p'.psi_target) accum));
          List.concat_map ~f:(fun p' -> mk_new_problem ~p:p' subspec) accum)
    else []
  in
  p :: new_ps
;;

(* ============================================================================================= *)
(*                    MAIN ENTRY POINT                                                           *)
(* ============================================================================================= *)

let enumerate_p (p : psi_def) =
  let res = mk_extra_toplevel_problems p in
  Log.info Fmt.(fun fmt () -> pf fmt "%i subproblems." (List.length res));
  res
;;
