open Base
open Common
open Lang
open ProblemDefs
open Env
open Term
open Utils

let join_repair (r1 : repair) (r2 : repair) =
  match r1, r2 with
  | AddRecursiveCalls tl1, AddRecursiveCalls tl2 -> AddRecursiveCalls (tl1 @ tl2)
  | AddRecursiveCalls tl, _ | _, AddRecursiveCalls tl -> AddRecursiveCalls tl
  | Lift, _ | _, Lift -> Lift
  | NoRepair, NoRepair -> NoRepair
;;

let find_matching_unknown (unknowns : VarSet.t) (v0 : variable) =
  if Set.mem unknowns v0
  then Some v0
  else (
    match String.split v0.vname ~on:'$' with
    | [ a; _ ] -> VarSet.find_by_name unknowns a
    | _ -> None)
;;

let is_shallow_value (t : term) (shallow : term) =
  match t.tkind with
  | TData (_, args) -> List.mem args ~equal:Terms.equal shallow
  | TApp (f, args) -> Terms.equal f shallow || List.mem args ~equal:Terms.equal shallow
  | TBin (_, a, b) -> Terms.(equal a shallow) || Terms.(equal b shallow)
  | TUn (_, a) -> Terms.equal a shallow
  | _ -> false
;;

let find_repair ~(ctx : env) ~(p : PsiDef.t) (witness : unrealizability_witness list) =
  Log.verbose_msg "All witnesses non-spurious, root causing...";
  let mk_eqn t =
    match
      Equations.make
        ~force_replace_off:true
        ~ctx
        ~p
        ~lifting:Lifting.empty_lifting
        (TermSet.singleton t)
    with
    | [ a ], _ -> a
    | a :: _, _ -> a
    | _ -> failwith "z"
  in
  let per_pair (uw : unrealizability_witness) =
    (* Compute original equations that do not hide some arguments. *)
    let eqn_i = mk_eqn uw.ci.witness_eqn.eterm
    and eqn_j = mk_eqn uw.ci.witness_eqn.eterm in
    let common_vars =
      Set.inter (VarMap.keyset uw.ci.witness_model) (VarMap.keyset uw.cj.witness_model)
    in
    let diff, _ =
      Set.fold common_vars ~init:([], []) ~f:(fun (accum, same) key ->
          let vi = Map.find_exn uw.ci.witness_model key in
          let vj = Map.find_exn uw.cj.witness_model key in
          if Terms.equal vi vj
          then accum, (key, vi) :: same
          else (key, (vi, vj)) :: accum, same)
    in
    let fv_r =
      Set.union
        (ctx >- Analysis.free_variables eqn_i.erhs)
        (ctx >- Analysis.free_variables eqn_j.erhs)
    in
    let classify_diff (v, (_val1, _val2)) =
      let rhs_args, xi =
        ( Set.diff fv_r p.PsiDef.target.psyntobjs
        , Set.max_elt (Set.inter fv_r p.PsiDef.target.psyntobjs) )
      in
      if not (Set.mem rhs_args v) (* Argument is not in function args. *)
      then (
        let t0 = uw.ci.witness_eqn.eterm in
        match
          ( List.find uw.ci.witness_eqn.eelim ~f:(fun (_, tscalar) ->
                Set.mem (ctx >- Analysis.free_variables tscalar) v)
          , xi )
        with
        | Some (trec, _), Some xi ->
          AddRecursiveCalls
            [ t0, xi, mk_app (mk_var ctx.ctx p.PsiDef.target.pmain_symb) [ trec ] ]
        | _, Some xi -> AddRecursiveCalls [ t0, xi, mk_var ctx.ctx v ]
        | _ -> NoRepair)
      else Lift
    in
    let diff_vars_ca =
      let s =
        Set.union
          (Set.diff
             (VarMap.keyset uw.ci.witness_model)
             (VarMap.keyset uw.cj.witness_model))
          (Set.diff
             (VarMap.keyset uw.cj.witness_model)
             (VarMap.keyset uw.ci.witness_model))
      in
      if Set.length s > 0 then Lift else NoRepair
    in
    diff_vars_ca :: List.map ~f:classify_diff diff
  in
  let cause =
    let all_repairs = List.concat_map ~f:per_pair witness in
    List.fold_left ~init:NoRepair ~f:join_repair all_repairs
  in
  Log.verbose
    Fmt.(fun fmt () -> pf fmt "Repair proposed: %s" (Pretty.str_of_repair cause));
  cause
;;
