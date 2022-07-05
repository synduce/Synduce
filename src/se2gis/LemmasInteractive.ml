open Base
open Common
open Witnesses
open Elim
open Env
open Lang
open ProblemDefs
open Term
open Utils
open SmtInterface

let make_term_info ~(ctx : env) ~(p : PsiDef.t) (term : term) : term_info * cond_lemma =
  let ti_elim, ti_formals = recurs_elim_of_term ~ctx ~p term in
  let input_args_t = List.map ~f:(var_type ctx) ti_formals in
  let lemma_f =
    Variable.mk
      ctx.ctx
      ~t:(Some (RType.fun_typ_pack input_args_t TBool))
      (Alpha.fresh ~s:"lemma" ctx.ctx.names)
  in
  ( { ti_context = ctx
    ; ti_psi_id = p.id
    ; ti_flag = true
    ; ti_term = term
    ; ti_func = lemma_f
    ; ti_elim
    ; ti_formals
    }
  , { cl_flag = true
    ; cl_cond = None
    ; cl_negatives = []
    ; cl_positives = []
    ; cl_lemmas = []
    } )
;;

let classify_witnesss_opt ~ctx witness : witness list Lwt.t =
  let f witness =
    Log.info (fun frmt () ->
        Fmt.(
          pf
            frmt
            "Classify this counterexample: %a (P/N/U)"
            (box (Pretty.pp_witness ~ctx))
            witness));
    match Stdio.In_channel.input_line Stdio.stdin with
    | Some "N" ->
      { witness with
        witness_stat = add_cause witness.witness_stat ViolatesTargetRequires
      }
    | Some "P" -> { witness with witness_stat = Valid }
    | _ -> witness
  in
  Lwt.return (List.map ~f witness)
;;

let set_term_lemma
    ~(ctx : env)
    ~(p : PsiDef.t)
    ~(key : term * term option)
    ~(lemma : term)
    : unit
  =
  match Predicates.find ~ctx ~key:(first key) with
  | None ->
    let ti, cl = make_term_info ~ctx ~p (fst key) in
    Predicates.add ~ctx ~key:(first key) ~data:(ti, { cl with cl_lemmas = [ lemma ] })
  | Some (ti, cls) ->
    let repl = ref false in
    let nl =
      List.map
        ~f:(fun cl ->
          if Option.equal Terms.equal cl.cl_cond (second key)
          then (
            repl := true;
            { cl with cl_lemmas = [ lemma ] })
          else cl)
        cls
    in
    if !repl
    then Predicates.set ~ctx ~key:(first key) ~data:(ti, nl)
    else (
      let new_elt =
        let _, cl = make_term_info ~ctx ~p (fst key) in
        { cl with cl_lemmas = [ lemma ] }
      in
      Predicates.set ~ctx ~key:(first key) ~data:(ti, new_elt :: nl))
;;

let add_lemmas ~(ctx : env) ~(p : PsiDef.t) (lstate : refinement_loop_state) : unit =
  let env_in_p = VarSet.of_list p.PsiDef.reference.pargs in
  let f t =
    let vars = Set.union (ctx >- Analysis.free_variables t) env_in_p in
    let env = VarSet.to_env vars in
    Log.info (fun frmt () ->
        Fmt.pf frmt "Please provide a constraint for \"@[%a@]\"." (pp_term ctx.ctx) t);
    Log.verbose (fun frmt () ->
        Fmt.pf
          frmt
          "Environment:@;@[functions %s, %s and %s@]@;and @[%a@]."
          p.PsiDef.reference.pvar.vname
          p.PsiDef.target.pvar.vname
          p.PsiDef.repr.pvar.vname
          (VarSet.pp ctx.ctx)
          vars);
    match Stdio.In_channel.input_line Stdio.stdin with
    | None | Some "" ->
      Log.info (fun frmt () -> Fmt.pf frmt "No additional constraint provided.");
      ()
    | Some x ->
      let smtterm =
        try
          let sexpr = Sexplib.Sexp.of_string x in
          Smtlib.SmtLib.smtTerm_of_sexp sexpr
        with
        | Failure _ -> None
      in
      let pred_term = ctx >>- term_of_smt env in
      let term x =
        match Predicates.get_with_precond ~ctx ~p ~key:(t, None) with
        | None -> pred_term x
        | Some inv -> mk_bin Binop.And inv (pred_term x)
      in
      (match smtterm with
      | None -> ()
      | Some x -> set_term_lemma ~ctx ~p ~key:(t, None) ~lemma:(term x))
  in
  Set.iter ~f lstate.t_set
;;

let parse_interactive_positive_example (det : term_info) (input : string) : term VarMap.t =
  List.fold
    ~init:VarMap.empty
    ~f:(fun acc s_ ->
      let s = Str.split (Str.regexp " *= *") s_ in
      if not (equal (List.length s) 2)
      then acc
      else (
        let key = trim (List.nth_exn s 0) in
        let data = mk_const (CInt (Int.of_string (trim (List.nth_exn s 1)))) in
        match VarSet.find_by_name (VarSet.of_list det.ti_formals) key with
        | None -> acc
        | Some var -> Map.set ~data acc ~key:var))
    (Str.split (Str.regexp " *, *") input)
;;

let interactive_get_positive_examples ~(ctx : Context.t) (det : term_info) =
  let vars =
    Set.filter
      ~f:(fun var ->
        match Variable.vtype ctx var with
        | None -> false
        | Some t -> not (RType.is_recursive ctx.types t))
      (Analysis.free_variables ~ctx det.ti_term)
  in
  Log.info (fun f () ->
      Fmt.(
        pf
          f
          "Enter an example as \"%s\""
          (String.concat
             ~sep:", "
             (List.map
                ~f:(fun var ->
                  var.vname
                  ^ "=<"
                  ^ (match Variable.vtype ctx var with
                    | None -> ""
                    | Some t ->
                      (match RType.base_name t with
                      | None -> ""
                      | Some tname -> tname))
                  ^ ">")
                (Set.elements vars)))));
  match Stdio.In_channel.input_line Stdio.stdin with
  | None -> []
  | Some s -> [ parse_interactive_positive_example det s ]
;;

let interactive_check_lemma ~ctx lemma_refinement_loop name vars det cl lemma_term
    : cond_lemma option Lwt.t
  =
  Log.info (fun f () ->
      Fmt.(
        pf
          f
          "Is the lemma \"%s %s = @[%a@]\" for term %a[%a] correct? [Y/N]"
          name
          (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vars))
          (pp_term ctx)
          lemma_term
          (pp_term ctx)
          det.ti_term
          (pp_subs ctx)
          det.ti_elim));
  match Stdio.In_channel.input_line Stdio.stdin with
  | Some "Y" ->
    let lemma =
      match cl.cl_cond with
      | None -> lemma_term
      | Some pre -> mk_bin Binop.Or (mk_un Unop.Not pre) lemma_term
    in
    Lwt.return (Some { cl with cl_lemmas = lemma :: cl.cl_lemmas })
  | _ ->
    Log.info (fun f () ->
        Fmt.(
          pf
            f
            "Would you like to provide a non-spurious example in which the lemma is \
             false? [Y/N]"));
    (match Stdio.In_channel.input_line Stdio.stdin with
    | Some "Y" ->
      lemma_refinement_loop
        { cl with
          cl_positives = cl.cl_positives @ interactive_get_positive_examples ~ctx det
        }
    | _ -> Lwt.return None)
;;
