(**  This module packages a few messages printed by the tool. *)
open Lib

open Base
open Fmt
open Common
open Env
open ProblemDefs
open Lang
open Term
open Utils

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

let find_missing_argument
    ~ctx
    (pb : PsiDef.t)
    (diff : (variable * (term * term)) list)
    (c : witness)
  =
  let msg_missing_arg rec_case u t =
    let msg =
      if (not rec_case) || is_shallow_value c.witness_eqn.eterm t
      then "."
      else "It may help to add the recursive call having access to that value."
    in
    Log.info (fun fmt () ->
        pf
          fmt
          "@[On input %a, %a should have access to %a@;%a"
          (ctx @>- pp_term)
          c.witness_eqn.eterm
          (ctx @>- VarSet.pp_var_names)
          u
          (ctx @>- pp_term)
          t
          string
          msg)
  in
  let fv = ctx >- Analysis.free_variables c.witness_eqn.erhs in
  let rhs_args = Set.diff fv pb.PsiDef.target.psyntobjs in
  let unknowns_in_use =
    VarSet.filter_map fv ~f:(find_matching_unknown pb.PsiDef.target.psyntobjs)
  in
  let say_diff (v, (_val1, _val2)) =
    if not (Set.mem rhs_args v)
    then (
      match
        List.find c.witness_eqn.eelim ~f:(fun (_, tscalar) ->
            Set.mem (ctx >- Analysis.free_variables tscalar) v)
      with
      | Some (trec, _) ->
        msg_missing_arg
          true
          unknowns_in_use
          (mk_app (mk_var ctx.ctx pb.PsiDef.target.pmain_symb) [ trec ])
      | _ -> msg_missing_arg false unknowns_in_use (mk_var ctx.ctx v))
    else ()
  in
  let pargs_diffs, pnonargs_diff =
    List.partition_tf diff ~f:(fun (v, _) ->
        Variable.(List.mem ~equal pb.PsiDef.target.pargs v))
  in
  match pnonargs_diff with
  | [] -> List.iter pargs_diffs ~f:say_diff
  | _ -> List.iter pnonargs_diff ~f:say_diff
;;

let find_missing_delta ~ctx (pb : PsiDef.t) (witness : unrealizability_witness) =
  let g = mk_var ctx.ctx pb.PsiDef.target.pmain_symb in
  let summ c =
    let f x = ctx >- Eval.in_model ~no_simplify:x c.witness_model in
    let cinput = f false c.witness_eqn.eterm in
    let celim_str =
      let conc_elims =
        List.map c.witness_eqn.eelim ~f:(fun (trec, telims) ->
            Terms.(mk_app g [ trec ] == (ctx >- Eval.in_model c.witness_model telims)))
      in
      match conc_elims with
      | [] -> ""
      | _ -> Fmt.str " with %a" (list ~sep:comma (ctx @>- pp_term)) conc_elims
    in
    Log.info (fun fmt () ->
        pf
          fmt
          "@[On input %a%s, the constraint@;@[(%a = %a)@]@;states@;@[%a = %a@]@]"
          (styled `Italic (ctx @>- pp_term))
          cinput
          celim_str
          (ctx @>- pp_term)
          (f true c.witness_eqn.erhs)
          (ctx @>- pp_term)
          (f true c.witness_eqn.elhs)
          (ctx @>- pp_term)
          (f false c.witness_eqn.erhs)
          (ctx @>- pp_term)
          (f false c.witness_eqn.elhs))
  in
  let fv =
    Set.union
      (ctx >- Analysis.free_variables witness.ci.witness_eqn.erhs)
      (ctx >- Analysis.free_variables witness.cj.witness_eqn.erhs)
  in
  let unknowns_in_use =
    VarSet.filter_map fv ~f:(find_matching_unknown pb.PsiDef.target.psyntobjs)
  in
  summ witness.ci;
  summ witness.cj;
  Log.info (fun fmt () ->
      pf
        fmt
        "There is no function %a that can satisfy these constraints."
        (styled (`Fg `Blue) (styled `Italic (ctx @>- VarSet.pp_var_names)))
        unknowns_in_use)
;;

(** When we get a witness of unrealizability, we need to explain why the problem is unrealizable.
  This function contains heuristics to root cause th problem an guide the user.
*)
let when_unrealizable ~ctx pb (witnesss : unrealizability_witness list) : unit =
  Log.(info (wrap "💡 Explanation: "));
  let f witness =
    let common_vars =
      Set.inter
        (VarMap.keyset witness.ci.witness_model)
        (VarMap.keyset witness.cj.witness_model)
    in
    let diff =
      Set.fold common_vars ~init:[] ~f:(fun accum key ->
          let vi = Map.find_exn witness.ci.witness_model key in
          let vj = Map.find_exn witness.cj.witness_model key in
          if Terms.equal vi vj then accum else (key, (vi, vj)) :: accum)
    in
    let ti = substitution witness.ci.witness_eqn.eelim witness.ci.witness_eqn.eterm
    and tj = substitution witness.cj.witness_eqn.eelim witness.cj.witness_eqn.eterm in
    if Terms.equal ti tj
    then
      (* Case 1: unknown is missing an argument. *)
      find_missing_argument ~ctx pb diff witness.ci
    else (
      match diff with
      | [] -> find_missing_delta ~ctx pb witness
      | _ ->
        Log.(
          let pp_term = ctx @>- pp_term
          and pp_equation = ctx >- Pretty.pp_equation in
          info (fun fmt () ->
              pf
                fmt
                "@[@[Terms: %a vs %a@].@;\
                 [@Equations: %a vs %a]@;\
                 @[witness differs in %a.@]@]"
                pp_term
                ti
                pp_term
                tj
                pp_equation
                witness.ci.witness_eqn
                pp_equation
                witness.cj.witness_eqn
                Fmt.(
                  list
                    (parens
                       (pair
                          (ctx @>- Variable.pp)
                          ~sep:colon
                          (pair pp_term ~sep:comma pp_term))))
                diff)))
  in
  List.iter ~f witnesss
;;
