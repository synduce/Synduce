open Base
open Lang
open Lang.Term
open Lang.Rewriter
open Utils

let contains_ebox (e : Expression.t) : bool =
  Expression.(
    reduce ~init:false ~join:( || )
      ~case:(fun _ e -> match e with EBox _ -> Some true | _ -> None)
      e)

let is_boxable_expr ((_, box_args) : int * IS.t) (t : Expression.t) : bool =
  let fv = Expression.free_variables t in
  if contains_ebox t then false else (not (Set.is_empty fv)) && Set.is_subset fv ~of_:box_args

let assign_match_box ((bid, bargs) : int * IS.t) ~(of_ : Expression.t) :
    (Expression.t * (int * Expression.t)) option =
  let boxed = ref None in
  let case _ e0 =
    if is_boxable_expr (bid, bargs) e0 then (
      match !boxed with
      | Some b -> if Expression.equal b e0 then Some (Expression.EBox bid) else None
      | None ->
          boxed := Some e0;
          Some (Expression.EBox bid))
    else None
  in
  let e' = Expression.transform case of_ in
  match !boxed with Some b -> Some (e', (bid, b)) | None -> None

module Solver = struct
  let max_deduction_attempts = 10

  let assign_const ((vid, _) : int * IS.t) : int * Expression.t =
    match Expression.get_var vid with
    | Some v -> (vid, Expression.get_ty_const (Variable.vtype_or_new v))
    | None -> (vid, Expression.get_ty_const RType.TInt)

  let functionalize ~(args : Expression.t list) ~(lemma : Expression.t option) (res : Expression.t)
      (boxes : (int * IS.t) list) : (int * Expression.t) list option =
    Fmt.(
      pf stdout "@[=== Solve Func. Equation: F %a = %a@]@." (list ~sep:sp Expression.pp) args
        Expression.pp res);
    let box_ids = IS.of_list (fst (List.unzip boxes)) in
    let box_args, bound_args =
      List.partition_map args ~f:(function
        | EVar vid when Set.mem box_ids vid -> (
            match List.Assoc.find boxes ~equal vid with
            | Some is -> Either.First (vid, is)
            | None -> Either.Second (Expression.mk_e_var vid))
        | _ as e -> Either.Second e)
    in
    Fmt.(
      pf stdout "\t@[Box args: %a@]@." (list ~sep:comma (parens (pair ~sep:sp int IS.pp))) box_args);
    Fmt.(pf stdout "\t@[Bound args: %a@]@." (list ~sep:comma Expression.pp) bound_args);
    let rec floop i (unassigned_bs, assigned_bs) (unassigned_bound, _q) res =
      let i' = i + 1 in
      if i' > max_deduction_attempts then None
      else if IS.(?.(Expression.free_variables res)) then
        (* The expression is a function of the bound arguments.
           If there are unassigned boxes left, assign a constant.
        *)
        Some (assigned_bs @ List.map ~f:assign_const unassigned_bs)
      else
        match unassigned_bound with
        | hd :: tl -> (
            Fmt.(
              pf stdout "@[\t\tTry to %a %a.@]@."
                (styled (`Bg `Magenta) string)
                "match" Expression.pp hd);
            match match_as_subexpr ~lemma hd ~of_:res with
            | Some (_, res') ->
                Fmt.(
                  pf stdout "@[\t\t\t✅  %a =@;%a <- (%a)@]@." Expression.pp res Expression.pp res'
                    Expression.pp hd);
                floop i' (unassigned_bs, assigned_bs) (tl, _q) res'
            | None ->
                Fmt.(pf stdout "\t\t\t❌@.");
                floop i' (unassigned_bs, assigned_bs) (tl, _q @ [ hd ]) res)
        | [] -> (
            match unassigned_bs with
            | hd :: tl -> (
                Fmt.(
                  pf stdout "@[\t\tTry to %a %a.@]@."
                    (styled (`Bg `Cyan) string)
                    "assign"
                    (pair ~sep:comma Expression.pp_ivar Expression.pp_ivarset)
                    hd);
                match assign_match_box hd ~of_:res with
                | Some (res', hd_assignment) ->
                    Fmt.(
                      pf stdout "@[\t\t\t✅ %a =@;%a <- %a@]@." Expression.pp res Expression.pp
                        res'
                        (parens (pair ~sep:comma int Expression.pp))
                        hd_assignment);
                    floop i' (tl, assigned_bs @ [ hd_assignment ]) (_q, []) res'
                | None ->
                    Fmt.(pf stdout "\t\t\t❌@.");
                    floop i' (tl @ [ hd ], assigned_bs) (_q, []) res)
            | [] -> floop i' ([], assigned_bs) (_q, []) res)
    in
    floop 0 (box_args, []) (bound_args, []) res

  (** "solve" a functional equation.
    Converts the term equation into a functionalization problem with Expressions.
    *)
  let functional_equation ~(func_side : term list) ~(lemma : term option) (res_side : term)
      (boxes : (variable * VarSet.t) list) : (variable * term) list =
    let flat_args =
      List.concat_map ~f:(fun t -> match t.tkind with TTup tl -> tl | _ -> [ t ]) func_side
    in
    let expr_args_o, expr_res_o =
      (all_or_none (List.map ~f:Expression.of_term flat_args), Expression.of_term res_side)
    in
    let lemma = Option.bind ~f:Expression.of_term lemma in
    (* No boxes : no lifting to compute. *)
    if List.is_empty boxes then []
    else
      let ided_boxes =
        List.map ~f:(fun (v, vas) -> (v.vid, IS.of_list (VarSet.vids_of_vs vas))) boxes
      in
      match (expr_args_o, expr_res_o) with
      | Some args, Some expr_res -> (
          match functionalize ~lemma ~args expr_res ided_boxes with
          | Some l -> (
              let l' =
                List.map l ~f:(fun (i, e) ->
                    Option.Let_syntax.(
                      let%bind v = Expression.get_var i in
                      let%bind e' = Expression.to_term e in
                      Some (v, e')))
              in
              match all_or_none l' with
              | Some l'' -> l''
              | None ->
                  Log.error_msg "Failed to deduce lifting.";
                  [])
          | None ->
              Log.error_msg "Failed to deduce lifting.";
              [])
      | _ ->
          Log.error_msg "Term is not a function-free expression, cannot deduce.";
          []
end
