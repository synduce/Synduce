open AState
open Base
open Lang
open Term
open PMRS
open Utils

(** A type for refinement location. *)
type rloc =
  { rid : int (**  The rule index. *)
  ; rxi : variable (** Each location is associated to a variable. *)
  ; rtokens : VarSet.t (** Each location has some tokens. *)
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
type extension_point =
  | Deconstr of psi_def * rloc * variable
  | Simple of psi_def * rloc * term list
  | NoExt of psi_def

let get_extension_psi = function
  | Deconstr (p, _, _) -> p
  | Simple (p, _, _) -> p
  | NoExt p -> p
;;

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

let mk_new_rule xi (nt, args, decons_arg, rhs) (constrname, constrargs) =
  let pat_vars =
    List.map
      ~f:(fun typ -> Variable.mk ~t:(Some typ) (Alpha.fresh ~s:decons_arg.vname ()))
      constrargs
  in
  let new_scalars =
    List.filter ~f:(fun v -> RType.is_base (Variable.vtype_or_new v)) pat_vars
  in
  let pat = PatConstr (constrname, List.map ~f:mk_pat_var pat_vars) in
  let new_args = List.map ~f:mk_var new_scalars in
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

let with_deconstruction ~(p : psi_def) (r : rloc) (dec : variable) =
  let deconstruct_one (nt, lhs_args, _, rhs) v =
    match RType.get_variants (Variable.vtype_or_new v) with
    | _ :: _ as cases ->
      let last_arg = List.last_exn lhs_args in
      let other_args = List.drop_last_exn lhs_args in
      let new_cases =
        List.map ~f:(mk_new_rule r.rxi (nt, other_args, last_arg, rhs)) cases
      in
      let new_rules =
        match new_cases with
        (* No constructor. *)
        | [] -> None
        (* Datatype has a single constructor. *)
        | [ (a, b) ] ->
          Some (VarSet.singleton a, Map.set p.psi_target.prules ~key:r.rid ~data:b)
        (* Multiple rules need to be addded to the PMRS. *)
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
      in
      (match new_rules with
      | Some (n_xis, nr) ->
        let psyntobjs = safe_remove_unknown r p in
        Some
          { p with
            psi_id = new_psi_id ()
          ; psi_target =
              { p.psi_target with psyntobjs = Set.union n_xis psyntobjs; prules = nr }
          }
      | None -> None)
    | _ -> None
    (* No variants, nothing to be done here. *)
  in
  Option.(
    bind ~f:(fun rrule -> deconstruct_one rrule dec) (Map.find p.psi_target.prules r.rid))
;;

let without_deconstruction p r (args : term list) = fst (mk_with_constant_args ~p r args)

let apply_extension : extension_point -> psi_def list = function
  | Deconstr (p, r, d) -> Option.to_list (with_deconstruction ~p r d)
  | Simple (p, r, args) -> without_deconstruction p r args
  | NoExt p -> [ p ]
;;

let mk_with_rec_args ~(p : psi_def) (r : rloc) : extension_point list =
  if Set.is_empty r.rtokens
  then []
  else (
    (* Analayse whether the arguments are usable as-is or need to be unpacked. *)
    let args_require_deconstruction, args_usable = analyze_rec_args_at_loc p r in
    let arg_choices =
      List.map
        ~f:(fun (f, args) -> mk_app (mk_var f) args)
        (applicable_nonterminals p (TermSet.of_varset args_usable))
    in
    List.map ~f:(fun x -> Simple (p, r, x)) (subsets arg_choices)
    @ List.map ~f:(fun x -> Deconstr (p, r, x)) (Set.elements args_require_deconstruction))
;;

let mk_new_problem ~(p : psi_def) (r : rloc) : extension_point list =
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
  List.map ~f:(fun x -> NoExt x) with_extra_constant_args
  @
  match with_extra_constant_args with
  | _ :: _ ->
    mk_with_rec_args ~p:(List.last_exn with_extra_constant_args) updated_objective
  | [] -> mk_with_rec_args ~p r
;;

let find_extensions (p : psi_def) =
  let g = p.psi_target in
  let paramset = VarSet.of_list g.pargs in
  (* Check for each rule.  *)
  let f ((rule_id, rrule) : int * rewrite_rule) =
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
            else [ { rid = rule_id; rxi; rtokens = more_args } ])
          (Set.elements unknowns))
      else [])
    else []
  in
  List.concat_map ~f (Map.to_alist g.prules)
;;

let list_extensions (p : psi_def) =
  let subspecs = find_extensions p in
  let new_ps =
    if List.length subspecs > 0
    then
      List.fold_left subspecs ~init:[ NoExt p ] ~f:(fun accum subspec ->
          Log.verbose
            Fmt.(
              fun fmt () ->
                pf
                  fmt
                  "@.༆ Apply %a to:@.%a@."
                  _pp_rloc
                  subspec
                  (list (PMRS.pp ~short:false))
                  (List.map ~f:(fun p' -> (get_extension_psi p').psi_target) accum));
          List.concat_map
            ~f:(fun p' -> mk_new_problem ~p:(get_extension_psi p') subspec)
            accum)
    else []
  in
  NoExt p :: new_ps
;;

(* ============================================================================================= *)
(*                    MAIN ENTRY POINT                                                           *)
(* ============================================================================================= *)

open Graph
open Pack

let _enumeration_graph = Graph.create ~size:20 ()
let _problem_versions : (int, psi_def) Hashtbl.t = Hashtbl.create ~size:20 (module Int)

let enumerate_p (p : psi_def) =
  let exts = list_extensions p in
  let res = List.concat_map ~f:apply_extension exts in
  Log.info Fmt.(fun fmt () -> pf fmt "%i subproblems." (List.length res));
  res
;;
