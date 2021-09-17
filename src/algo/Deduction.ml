open Base
open Lang
open Lang.Term
open Lang.Rewriter
open Utils
open Fmt

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
  let open Expression in
  let boxed = ref None in
  let box_it (e0 : t) (bid : t) =
    match !boxed with
    | Some b -> if equal b e0 then Some bid else None
    | None ->
        boxed := Some e0;
        Some bid
  in
  let case _ e0 =
    (* Case 1 : the expression is immediately "boxable" *)
    if is_boxable_expr (bid, bargs) e0 then box_it e0 (EBox bid)
    else
      (* Case 2: some subexpression in an EOp are "boxable". *)
      match e0 with
      | EOp (op, args) -> (
          match List.partition_tf ~f:(is_boxable_expr (bid, bargs)) args with
          | b1 :: bs, u1 :: us when is_commutative op ->
              box_it (mk_e_assoc op (b1 :: bs)) (mk_e_assoc op (EBox bid :: u1 :: us))
          | _, [] -> None (*This case should have been detected in 1. *)
          | _ -> None)
      | _ -> None
  in
  let e' = Expression.transform case of_ in
  match !boxed with Some b -> Some (e', (bid, b)) | None -> None

module Solver = struct
  let max_deduction_attempts = 10

  let assign_const ((vid, _) : int * IS.t) : int * Expression.t =
    match Expression.get_var vid with
    | Some v -> (vid, Expression.get_ty_const (Variable.vtype_or_new v))
    | None -> (vid, Expression.get_ty_const RType.TInt)

  type deduction_loop_state = {
    expression : Expression.t;
    free_boxes : (int * IS.t) list;
    full_boxes : (int * Expression.t) list;
    free_bound_exprs : Expression.t list;
    queue : Expression.t list;
  }

  let deduction_loop ~(lemma : Expression.t option) (box_args : (int * IS.t) list)
      (bound_args : Expression.t list) (expr : Expression.t) :
      ( (int * Expression.t) list,
        (* Return assignment from box id to expression. *)
        deduction_loop_state )
      (* Error returns information where the loop stopped. *)
      Result.t =
    let rec floop i (state : deduction_loop_state) =
      let i' = i + 1 in
      if i' > max_deduction_attempts then Error state
      else if IS.(?.(Expression.free_variables state.expression)) then
        (* The expression is a function of the bound arguments.
           If there are unassigned boxes left, assign a constant.
        *)
        Ok (state.full_boxes @ List.map ~f:assign_const state.free_boxes)
      else
        match state.free_bound_exprs with
        | hd :: tl -> (
            Log.verbose
              (Log.wrap2 "@[\t\tTry to %a %a.@]"
                 (styled (`Bg `Magenta) string)
                 "match" Expression.pp hd);
            match match_as_subexpr ~lemma hd ~of_:state.expression with
            | Some (id, res') ->
                Log.verbose (fun fmt () ->
                    pf fmt "@[\t\t[-> %i]✅  %a =@;%a <- (%a)@]" id Expression.pp state.expression
                      Expression.pp res' Expression.pp hd);
                floop i' { state with expression = res'; free_bound_exprs = tl }
            | None ->
                Log.verbose_msg "\t\t\t❌";
                floop i' { state with free_bound_exprs = tl; queue = state.queue @ [ hd ] })
        | [] -> (
            match state.free_boxes with
            | hd :: tl -> (
                Log.verbose (fun fmt () ->
                    pf fmt "@[\t\tTry to %a %a.@]@."
                      (styled (`Bg `Cyan) string)
                      "assign"
                      (pair ~sep:comma Expression.pp_ivar Expression.pp_ivarset)
                      hd);
                match assign_match_box hd ~of_:state.expression with
                | Some (res', (hd_id, hd_e)) ->
                    Log.verbose (fun fmt () ->
                        pf fmt "@[\t\t[-> %i]✅ %a =@;%a <- %a@]" hd_id Expression.pp
                          state.expression Expression.pp res'
                          (parens (pair ~sep:comma int Expression.pp))
                          (hd_id, hd_e));
                    floop i'
                      {
                        expression = res';
                        free_boxes = tl;
                        full_boxes = (hd_id, hd_e) :: state.full_boxes;
                        free_bound_exprs = state.queue;
                        queue = [];
                      }
                | None ->
                    Log.verbose_msg "\t\t\t❌@.";
                    floop i'
                      {
                        state with
                        free_boxes = tl @ [ hd ];
                        free_bound_exprs = state.queue;
                        queue = [];
                      })
            | [] ->
                floop i' { state with free_boxes = []; free_bound_exprs = state.queue; queue = [] })
    in
    floop 0
      {
        expression = expr;
        free_boxes = box_args;
        full_boxes = [];
        free_bound_exprs = bound_args;
        queue = [];
      }

  let functionalize ~(args : Expression.t list) ~(lemma : Expression.t option) (res : Expression.t)
      (boxes : (int * IS.t) list) :
      ((int * Expression.t) list, (int * Expression.t) list * Expression.t) Result.t =
    Log.verbose
      Fmt.(
        Log.wrap2 "@[=== Solve Func. Equation:@;@[<h 2>@[F %a@] =@;@[%a@]@]@]@."
          (list ~sep:sp (parens Expression.pp))
          args Expression.pp res);
    let box_ids = IS.of_list (fst (List.unzip boxes)) in
    let box_args, bound_args =
      List.partition_map args ~f:(function
        | EVar vid when Set.mem box_ids vid -> (
            match List.Assoc.find boxes ~equal vid with
            | Some is -> Either.First (vid, is)
            | None -> Either.Second (Expression.mk_e_var vid))
        | _ as e -> Either.Second e)
    in
    Log.verbose
      (Log.wrap1 "\t@[Box args: %a@]" (list ~sep:comma (parens (pair ~sep:sp int IS.pp))) box_args);
    Log.verbose (Log.wrap1 "\t@[Bound args: %a@]" (list ~sep:comma Expression.pp) bound_args);
    match deduction_loop ~lemma box_args bound_args res with
    | Ok x -> Ok x
    | Error state -> Error (state.full_boxes, state.expression)

  let resolve_box_bindings l =
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
        []

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
          | Ok l -> resolve_box_bindings l
          | Error (l, e_leftover) ->
              Log.error
                Fmt.(
                  fun fmt () ->
                    pf fmt "Failed to deduce lifting, remaining %a" Expression.pp e_leftover);
              resolve_box_bindings l)
      | _ ->
          Log.error_msg
            (Fmt.str "Term {%a} %a is not a function-free expression, cannot deduce."
               (list ~sep:comma pp_term)
               (List.map ~f:Reduce.reduce_term flat_args)
               pp_term res_side);
          []
end
