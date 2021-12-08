open AState
open Base
open Lang
open Term
open PMRS
open Utils

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

let mk_new_problem
    ~(p : psi_def)
    ((rid, rrule, xi, tokens) : int * rewrite_rule * variable * VarSet.t)
    : psi_def list
  =
  let with_extra_constant_args =
    let cargs =
      Set.elements
        (Set.filter tokens ~f:(fun v -> RType.is_base (Variable.vtype_or_new v)))
    in
    if List.length cargs > 0
    then (
      let ext_xi =
        let in_t, re_t = RType.fun_typ_unpack (Variable.vtype_or_new xi) in
        (* Input type is extended by adding arguments *)
        let t =
          Some (RType.fun_typ_pack (in_t @ List.map ~f:Variable.vtype_or_new cargs) re_t)
        in
        Variable.mk ~t (Alpha.fresh ~s:xi.vname ())
      in
      let add_arg_and_replace_unknown _t =
        let nt, lhs, pat, rhs = rrule in
        nt, lhs, pat, extend_function ~from:xi ~to_:ext_xi (List.map ~f:mk_var cargs) rhs
      in
      [ { p with
          psi_id = new_psi_id ()
        ; psi_target =
            { p.psi_target with
              (* TODO : check if unknown is present in other rules before! *)
              psyntobjs = Set.add (Set.remove p.psi_target.psyntobjs xi) ext_xi
            ; prules = Map.update p.psi_target.prules rid ~f:add_arg_and_replace_unknown
            }
        }
      ])
    else []
  in
  with_extra_constant_args
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
        List.map ~f:(fun xi -> i, rrule, xi, more_args) (Set.elements unknowns))
      else [])
    else []
  in
  let subspecs = List.concat_map ~f (Map.to_alist g.prules) in
  p :: List.concat_map ~f:(mk_new_problem ~p) subspecs
;;

let enumerate_p (p : psi_def) =
  let res = mk_extra_toplevel_problems p in
  Log.info Fmt.(fun fmt () -> pf fmt "%i subproblems." (List.length res));
  res
;;
