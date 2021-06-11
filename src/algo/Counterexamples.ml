open AState
open Base
open Lang
open Lang.Term
open Projection
open SmtInterface
open Smtlib
open Utils

(* ============================================================================================= *)
(*                               CHECKING UNREALIZABILITY                                        *)
(* ============================================================================================= *)

type unrealizability_ctex = { i : int; j : int; ci : ctex; cj : ctex }
(** A counterexample to realizability is a pair of models: a pair of maps from variable ids to terms. *)

let pp_unrealizability_ctex (frmt : Formatter.t) (uc : unrealizability_ctex) : unit =
  let pp_model frmt model =
    (* Print as comma-separated list of variable -> term *)
    Fmt.(list ~sep:comma (pair ~sep:Utils.rightarrow (option pp_variable) pp_term))
      frmt
      (List.map
         ~f:(fun (vid, t) -> (VarSet.find_by_id uc.ci.ctex_vars vid, t))
         (Map.to_alist model))
  in
  Fmt.(
    pf frmt "@[M<%i> = [%a]@]@;@[M'<%i> = [%a]@]" uc.i pp_model uc.ci.ctex_model uc.j pp_model
      uc.cj.ctex_model)

let reinterpret_model (m0i, m0j') var_subst =
  List.fold var_subst
    ~init:(Map.empty (module Int), Map.empty (module Int))
    ~f:(fun (m, m') (v, v') ->
      Variable.free v';
      match Map.find m0i v.vname with
      | Some data -> (
          let new_m = Map.set m ~key:v.vid ~data in
          match Map.find m0j' v'.vname with
          | Some data -> (new_m, Map.set m' ~key:v.vid ~data)
          | None -> (new_m, m'))
      | None -> (
          match Map.find m0j' v'.vname with
          | Some data -> (m, Map.set m' ~key:v.vid ~data)
          | None -> (m, m')))

(** Check if system of equations defines a functionally realizable synthesis problem.
  If any equation defines an unsolvable problem, an unrealizability_ctex is added to the
  list of counterexamples to be returned.
  If the returned list is empty, the problem may be solvable/realizable.
  If the returned list is not empty, the problem is not solvable / unrealizable.
*)
let check_unrealizable (unknowns : VarSet.t) (eqns : equation_system) : unrealizability_ctex list =
  Log.info (fun f () -> Fmt.(pf f "Checking unrealizability..."));
  let start_time = Unix.gettimeofday () in
  let solver = Solvers.make_z3_solver () in
  Solvers.load_min_max_defs solver;
  (* Main part of the check, applied to each equation in eqns. *)
  let check_eqn_accum (ctexs : unrealizability_ctex list) ((i, eqn_i), (j, eqn_j)) =
    let vseti =
      Set.diff
        (Set.union (Analysis.free_variables eqn_i.elhs) (Analysis.free_variables eqn_i.erhs))
        unknowns
    in
    let vsetj =
      Set.diff
        (Set.union (Analysis.free_variables eqn_j.elhs) (Analysis.free_variables eqn_j.erhs))
        unknowns
    in
    let var_subst = VarSet.prime vsetj in
    let vsetj' = VarSet.of_list (List.map ~f:snd var_subst) in
    let sub = List.map ~f:(fun (v, v') -> (mk_var v, mk_var v')) var_subst in
    (* Extract the arguments of the rhs, if it is a call to an unknown. *)
    let maybe_rhs_args =
      match (eqn_i.erhs.tkind, eqn_j.erhs.tkind) with
      | TApp ({ tkind = TVar f_v_i; _ }, args_i), TApp ({ tkind = TVar f_v_j; _ }, args_j) ->
          if
            Set.mem unknowns f_v_i && Set.mem unknowns f_v_j
            && Variable.(f_v_i = f_v_j)
            && List.length args_i = List.length args_j
          then
            let fv_args =
              VarSet.union_list (List.map ~f:Analysis.free_variables (args_i @ args_j))
            in
            (* Check there are no unknowns in the args. *)
            if Set.are_disjoint fv_args unknowns then Some (args_i, args_j) else None
          else None
      | _ -> None
    in
    match maybe_rhs_args with
    | None -> ctexs (* If we cannot match the expected structure, skip it. *)
    | Some (rhs_args_i, rhs_args_j) ->
        (* (push). *)
        Solvers.spush solver;
        (* Declare the variables. *)
        Solvers.declare_all solver (decls_of_vars (Set.union vseti vsetj'));
        (* Assert preconditions, if they exist. *)
        (match eqn_i.eprecond with
        | Some pre_i ->
            Solvers.declare_all solver (decls_of_vars (Analysis.free_variables pre_i));
            Solvers.smt_assert solver (smt_of_term pre_i)
        | None -> ());
        (match eqn_j.eprecond with
        | Some pre_j ->
            Solvers.declare_all solver (decls_of_vars (Analysis.free_variables pre_j));
            Solvers.smt_assert solver (smt_of_term (substitution sub pre_j))
        | None -> ());
        (* The lhs of i and j must be different. **)
        let lhs_diff =
          let projs = projection_eqns eqn_i.elhs (substitution sub eqn_j.elhs) in
          List.map ~f:(fun (lhs, rhs) -> mk_un Not (mk_bin Eq lhs rhs)) projs
        in
        List.iter lhs_diff ~f:(fun eqn -> Solvers.smt_assert solver (smt_of_term eqn));
        (* The rhs must be equal. *)
        List.iter2_exn rhs_args_i rhs_args_j ~f:(fun rhs_i_arg_term rhs_j_arg_term ->
            let rhs_eqs = projection_eqns rhs_i_arg_term (substitution sub rhs_j_arg_term) in
            List.iter rhs_eqs ~f:(fun (lhs, rhs) ->
                Solvers.smt_assert solver (smt_of_term (mk_bin Eq lhs rhs))));
        let new_ctexs =
          match Solvers.check_sat solver with
          | Sat -> (
              match Solvers.get_model solver with
              | SExps s ->
                  let model = model_to_constmap (SExps s) in
                  let m0i, m0j =
                    Map.partitioni_tf
                      ~f:(fun ~key ~data:_ -> Option.is_some (VarSet.find_by_name vseti key))
                      model
                  in
                  (* Remap the names to ids of the original variables in m' *)
                  let m_i, m_j = reinterpret_model (m0i, m0j) var_subst in
                  let ctex_i : ctex = { ctex_eqn = eqn_i; ctex_vars = vseti; ctex_model = m_i } in
                  let ctex_j : ctex = { ctex_eqn = eqn_j; ctex_vars = vsetj; ctex_model = m_j } in
                  { i; j; ci = ctex_i; cj = ctex_j } :: ctexs
              | _ -> ctexs)
          | _ -> ctexs
        in
        Solvers.spop solver;
        new_ctexs
  in
  let eqns_indexed = List.mapi ~f:(fun i eqn -> (i, eqn)) eqns in
  let ctexs = List.fold ~f:check_eqn_accum ~init:[] (combinations eqns_indexed) in
  Solvers.close_solver solver;
  let elapsed = Unix.gettimeofday () -. start_time in
  Log.info (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
  Log.info (fun f () ->
      match ctexs with
      | [] -> Fmt.pf f "No counterexample to realizability found."
      | _ :: _ ->
          Fmt.(
            pf f
              "@[Counterexamples found!@;\
               @[<hov 2>❔ Equations:@;\
               @[<v>%a@]@]@;\
               @[<hov 2>❔ Counterexample models:@;\
               @[<v>%a@]@]@]"
              (list ~sep:sp (box (pair ~sep:colon int (box pp_equation))))
              eqns_indexed
              (list ~sep:sep_and pp_unrealizability_ctex)
              ctexs));
  ctexs

(** Classify counterexamples into positive or negative counterexamples with respect
    to the Tinv predicate in the problem.
*)
let classify_ctexs ~(p : psi_def) (ctexs : ctex list) : ctex list * ctex list =
  let _ = p in
  let solver = Solvers.make_cvc4_solver () in
  Solvers.set_logic solver "DT_LIA";
  let f _ctex = true in
  let postives, negatives = List.partition_tf ~f ctexs in
  Solvers.close_solver solver;
  (postives, negatives)