open Base
open Lang
open Lang.Term
open Lang.Rewriter
open Utils
open Fmt

let as_unknown_app ?(match_functions = fun _ -> false) ~unknowns t : term list option =
  let rec aux t =
    match t.tkind with
    | TApp (f, [ arg ]) when match_functions f -> aux arg
    | TApp ({ tkind = TVar f; _ }, args) -> if Set.mem unknowns f then Some args else None
    | _ -> None
  in
  aux t
;;

let gather_args ~(unknowns : VarSet.t) (t : Term.term) : Expression.t list option =
  let flatten_args args =
    List.concat_map
      ~f:(fun t ->
        match t.tkind with
        | TTup tl -> tl
        | _ -> [ t ])
      args
  in
  Option.bind
    ~f:(fun l -> all_or_none (List.map ~f:Expression.of_term (flatten_args l)))
    (as_unknown_app ~unknowns t)
;;

(* ============================================================================================= *)
(*                           BOXING / UNBOXING                                                   *)
(* ============================================================================================= *)

let contains_ebox (e : Expression.t) : bool =
  Expression.(
    reduce
      ~init:false
      ~join:( || )
      ~case:(fun _ e ->
        match e with
        | EBox _ -> Some true
        | _ -> None)
      e)
;;

let is_boxable_expr ((_, box_args) : int * IS.t) (t : Expression.t) : bool =
  let fv = Expression.free_variables t in
  if contains_ebox t
  then false
  else (not (Set.is_empty fv)) && Set.is_subset fv ~of_:box_args
;;

let assign_match_box ((bid, bargs) : int * IS.t) ~(of_ : Expression.t)
    : (Expression.t * (int * Expression.t)) option
  =
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
    if is_boxable_expr (bid, bargs) e0
    then box_it e0 (EBox (Indexed bid))
    else (
      (* Case 2: some subexpression in an EOp are "boxable". *)
      match e0 with
      | EOp (op, args) ->
        (match List.partition_tf ~f:(is_boxable_expr (bid, bargs)) args with
        | b1 :: bs, u1 :: us when is_commutative op ->
          box_it
            (mk_e_assoc op (b1 :: bs))
            (mk_e_assoc op (EBox (Indexed bid) :: u1 :: us))
        | _, [] -> None (*This case should have been detected in 1. *)
        | _ -> None)
      | _ -> None)
  in
  let e' = Expression.transform case (Rewriter.factorize of_) in
  match !boxed with
  | Some b -> Some (e', (bid, b))
  | None -> None
;;

module Solver = struct
  let max_deduction_attempts = 10

  type deduction_loop_state =
    { expression : Expression.t
    ; free_boxes : (int * IS.t) list
    ; full_boxes : (int * Expression.t) list
    ; free_bound_exprs : (int * Expression.t) list
    ; queue : (int * Expression.t) list
    }

  let deduction_loop
      ?(verb = true)
      ~(lemma : Expression.t option)
      (box_args : (int * IS.t) list)
      (bound_args : (int * Expression.t) list)
      (expr : Expression.t)
      : ( (int * Expression.t) list * Expression.t
      , (* Return assignment from box id to expression. *)
        deduction_loop_state )
      (* Error returns information where the loop stopped. *)
      Result.t
    =
    let rec floop i (state : deduction_loop_state) =
      let i' = i + 1 in
      if i' > max_deduction_attempts
      then Error state
      else if IS.( ?. ) (Expression.free_variables state.expression)
      then
        (* The expression is a function of the bound arguments.
           If there are unassigned boxes left, assign a constant.
        *)
        Ok (state.full_boxes, state.expression)
      else (
        (* First try to remove subexpressions that match arguments of the equation. *)
        match state.free_bound_exprs with
        | (arg_id, arg_expr) :: tl ->
          (* There are some arguments to match. Try using the head first.  *)
          let arg_expr = Expression.simplify arg_expr in
          if verb
          then
            Log.verbose
              (Log.wrap2
                 "@[~ ~ Try to %a %a.@]"
                 (styled (`Bg `Magenta) string)
                 "match"
                 Expression.pp
                 arg_expr);
          (match
             match_as_subexpr ~lemma (Position arg_id) arg_expr ~of_:state.expression
           with
          | Some res' ->
            (* Some match; Discard the argument and box the subexpression.  *)
            if verb
            then
              Log.verbose (fun fmt () ->
                  pf
                    fmt
                    "@[~ ~ ✅  λ.%a.%a@;%a =@;%a@]"
                    Expression.pp
                    (Expression.EBox (Position arg_id))
                    (box Expression.pp)
                    res'
                    (box Expression.pp)
                    arg_expr
                    (box Expression.pp)
                    state.expression);
            floop i' { state with expression = res'; free_bound_exprs = tl }
          (* No match; keep the argument but queue it. It might match after rewriting steps.  *)
          | None ->
            if verb then Log.verbose_msg "~ ~ ~ ❌";
            floop
              i'
              { state with
                free_bound_exprs = tl
              ; queue = state.queue @ [ arg_id, arg_expr ]
              })
        | [] ->
          (* If there are no arguments in the queue, we start using free boxes.  *)
          (match state.free_boxes with
          | hd :: tl ->
            if verb
            then
              Log.verbose (fun fmt () ->
                  pf
                    fmt
                    "@[~ ~ Try to %a %a.@]@."
                    (styled (`Bg `Cyan) string)
                    "assign"
                    (pair ~sep:comma Expression.pp_ivar Expression.pp_ivarset)
                    hd);
            (match assign_match_box hd ~of_:state.expression with
            | Some (res', (hd_id, hd_e)) ->
              (* Some subexpression has the same free variables as the free box. Box it,
              a bind the box to that subexpression.
             *)
              if verb
              then
                Log.verbose (fun fmt () ->
                    pf
                      fmt
                      "@[~ ~ ✅ λ%a.%a@;%a=@;%a@]"
                      (box Expression.pp)
                      (Expression.EBox (Indexed hd_id))
                      (box Expression.pp)
                      res'
                      (box Expression.pp)
                      hd_e
                      (box Expression.pp)
                      state.expression);
              floop
                i'
                { expression = res'
                ; free_boxes = tl
                ; full_boxes = (hd_id, hd_e) :: state.full_boxes
                ; free_bound_exprs = state.queue
                ; queue = []
                }
            | None ->
              (* No match; there's a good change this problem has no solution, but try again with arguments.  *)
              if verb then Log.verbose_msg "~ ~ ~ ❌@.";
              floop
                i'
                { state with
                  free_boxes = tl @ [ hd ]
                ; free_bound_exprs = state.queue
                ; queue = []
                })
          | [] ->
            floop
              i'
              { state with free_boxes = []; free_bound_exprs = state.queue; queue = [] }))
    in
    floop
      0
      { expression = expr
      ; free_boxes = box_args
      ; full_boxes = []
      ; free_bound_exprs = bound_args
      ; queue = []
      }
  ;;

  (**
    [functionalize ~args ~lemma res boxes] extracts the function that computes res given
    the arguments args and using boxes to capture any remaining expressions.
  *)
  let functionalize
      ?(verb = true)
      ~(args : Expression.t list)
      ~(lemma : Expression.t option)
      (res : Expression.t)
      (boxes : (int * IS.t) list)
      : ( (int * Expression.t) list * Expression.t
      , (int * Expression.t) list * Expression.t ) Result.t
    =
    (* Print the equation to be solved. *)
    if verb
    then
      Log.verbose
        Fmt.(
          Log.wrap2
            "@[=== Solve Func. Equation:@;@[<h 2>@[F %a@] =@;@[%a@]@]@]"
            (list ~sep:sp (parens Expression.pp))
            args
            Expression.pp
            res);
    let box_ids = IS.of_list (fst (List.unzip boxes)) in
    let box_args, bound_args =
      List.partition_map args ~f:(function
          | EVar vid when Set.mem box_ids vid ->
            (match List.Assoc.find boxes ~equal vid with
            | Some is -> Either.First (vid, is)
            | None -> Either.Second (Expression.mk_e_var vid))
          | _ as e -> Either.Second e)
    in
    (* Print summary of arguments of the function. *)
    if verb
    then (
      Log.verbose
        (Log.wrap1
           "~ @[Box args: %a@]"
           (list ~sep:comma (parens (pair ~sep:sp int IS.pp)))
           box_args);
      Log.verbose
        (Log.wrap1 "~ @[Bound args: %a@]" (list ~sep:comma Expression.pp) bound_args));
    (* Start solving with a call to deduction_loop *)
    match
      deduction_loop
        ~verb
        ~lemma
        box_args
        (* Match large expressions first. *)
        (List.rev
           (List.sort
              ~compare:(fun (_, x) (_, x') -> Expression.size_compare x x')
              (index_list bound_args)))
        (* The target to functionalize. *)
        res
    with
    | Ok x -> Ok x
    | Error state -> Error (state.full_boxes, state.expression)
  ;;

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
      Log.error_msg "Failed to deduce box value.";
      []
  ;;

  (** Treat the guess expression as a function that needs to be applied to the arguments
    gathered from applying the same arg-collecting procedure a in the other deduction
    solving functions.
    *)
  let guess_application ~(unknowns : VarSet.t) (guess : Expression.t) (rhs : term)
      : term option
    =
    let maybe_args = gather_args ~unknowns rhs in
    Option.bind ~f:(fun args -> Expression.(apply guess args |> to_term)) maybe_args
  ;;

  let best_unification
      ~(unknowns : VarSet.t)
      (eqns : (term * term option * term * term) list)
      (guesses : Expression.t option list)
      : Skeleton.t option
    =
    let validate_via_solver guess =
      let open SmtInterface in
      let equations_to_check =
        let eqns' =
          List.map eqns ~f:(fun (_, pre, lhs, rhs) ->
              Option.map (guess_application ~unknowns guess rhs) ~f:(fun rhs' ->
                  let constr = mk_bin Binop.Eq lhs rhs' in
                  pre, mk_un Unop.Not constr))
        in
        match all_or_none eqns' with
        | Some eqns'' -> eqns''
        | None -> failwith "UNSAT"
      in
      let solver = SyncSmt.make_z3_solver () in
      let () =
        SyncSmt.exec_all
          solver
          (Commands.mk_preamble
             ~logic:(SmtLogic.infer_logic (List.map ~f:snd equations_to_check))
             ())
      in
      List.fold_until
        ~init:0
        equations_to_check
        ~f:(fun _ (pre, constr) ->
          SyncSmt.spush solver;
          let fv = Analysis.free_variables constr in
          SyncSmt.exec_all solver (Commands.decls_of_vars fv);
          (match pre with
          | Some precond -> SyncSmt.smt_assert solver (smt_of_term precond)
          | None -> ());
          SyncSmt.smt_assert solver (smt_of_term constr);
          match SyncSmt.check_sat solver with
          | Unsat ->
            SyncSmt.spop solver;
            Continue 0
          | _ ->
            SyncSmt.close_solver solver;
            Stop None)
        ~finish:(fun _ -> Some guess)
    in
    let validate_guess guess =
      if List.for_all guesses ~f:(function
             | Some x' -> Poly.equal guess x'
             | _ -> false)
      then Some guess
      else (
        try validate_via_solver guess with
        | _ -> None)
    in
    let guess_1 =
      let ok_guesses =
        let filter guess =
          match guess with
          (* Arbitrary cutoff for guess size *)
          | Some g -> if Expression.size g > 15 then None else Some g
          | None -> None
        in
        List.rev
          (List.sort ~compare:Expression.compare (List.filter_map ~f:filter guesses))
      in
      match ok_guesses with
      | hd :: _ -> Some hd
      | _ -> None
    in
    Option.(guess_1 >>= validate_guess >>= Skeleton.of_expression)
  ;;

  let solve_eqn
      ~(unknowns : VarSet.t)
      (xi : variable)
      (pre : term option)
      (lhs : term)
      (rhs : term)
      : Expression.t option
    =
    let open Option.Let_syntax in
    let%bind lhs_expr = Expression.of_term lhs in
    let pre_expr = Option.bind ~f:Expression.of_term pre in
    let%bind args = gather_args ~unknowns rhs in
    Log.verbose
      Fmt.(
        fun fmt () ->
          pf
            fmt
            "Deduction: @;@[%a =>@;@[%s(%a)@] =@;@[%a@]"
            (option Expression.pp)
            pre_expr
            xi.vname
            (list ~sep:comma Expression.pp)
            args
            Expression.pp
            lhs_expr);
    match functionalize ~verb:false ~args ~lemma:pre_expr lhs_expr [] with
    | Ok (_, r) ->
      let r = factorize r in
      Log.verbose Fmt.(fun fmt () -> pf fmt "Solution =@;%a" (box Expression.pp) r);
      Some r
    | Error _ ->
      Log.verbose Fmt.(fun fmt () -> pf fmt "No solution.");
      None
  ;;

  let presolve_equations
      ~(unknowns : VarSet.t)
      (eqns : (term * term option * term * term) list)
      (xi : variable)
      : Skeleton.t option
    =
    let f (_, pre, lhs, rhs) = solve_eqn ~unknowns xi pre lhs rhs in
    let guesses = List.map ~f eqns in
    best_unification ~unknowns eqns guesses
  ;;

  (** "solve" a functional equation with lifting.
    Converts the term equation into a functionalization problem with Expressions.
    *)
  let functional_equation
      ~(func_side : term list)
      ~(lemma : term option)
      (res_side : term)
      (boxes : (variable * VarSet.t) list)
      : (variable * term) list * Expression.t option
    =
    let flat_args =
      List.concat_map
        ~f:(fun t ->
          match t.tkind with
          | TTup tl -> tl
          | _ -> [ t ])
        func_side
    in
    let expr_args_o, expr_res_o =
      all_or_none (List.map ~f:Expression.of_term flat_args), Expression.of_term res_side
    in
    let lemma = Option.bind ~f:Expression.of_term lemma in
    (* No boxes : no lifting to compute. *)
    if List.is_empty boxes
    then [], None
    else (
      let ided_boxes =
        List.map ~f:(fun (v, vas) -> v.vid, IS.of_list (VarSet.vids_of_vs vas)) boxes
      in
      match expr_args_o, expr_res_o with
      | Some args, Some expr_res ->
        (match functionalize ~lemma ~args expr_res ided_boxes with
        | Ok (l, _) -> resolve_box_bindings l, None
        | Error (l, e_leftover) ->
          Log.error
            Fmt.(
              fun fmt () ->
                pf
                  fmt
                  "@[Failed to solve functional equation remaining expression is:@;%a@]"
                  Expression.pp
                  e_leftover);
          resolve_box_bindings l, Some e_leftover)
      | _ ->
        Log.error_msg
          (Fmt.str
             "Term {%a} %a is not a function-free expression, cannot deduce."
             (list ~sep:comma pp_term)
             (List.map ~f:Reduce.reduce_term flat_args)
             pp_term
             res_side);
        [], None)
  ;;
end
