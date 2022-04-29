open Base
open Common
open Counterexamples
open Elim
open Env
open Lang
open ProblemDefs
open LemmaVerif
open Term
open Utils
open SmtInterface

let make_term_info ~(ctx : env) ~(p : PsiDef.t) (term : term) : term_info =
  let recurs_elim, scalar_vars = recurs_elim_of_term ~ctx ~p term in
  let input_args_t = List.map ~f:(var_type ctx) scalar_vars in
  let lemma_f =
    Variable.mk
      ctx.ctx
      ~t:(Some (RType.fun_typ_pack input_args_t TBool))
      (Alpha.fresh ~s:"lemma" ctx.ctx.names)
  in
  { term
  ; splitter = None
  ; lemmas = []
  ; lemma = lemma_f
  ; lemma_candidate = None
  ; negative_ctexs = []
  ; positive_ctexs = []
  ; recurs_elim
  ; scalar_vars
  ; current_preconds = None
  }
;;

let classify_ctexs_opt ~ctx ctexs : ctex list =
  let f ctex =
    Log.info (fun frmt () ->
        Fmt.(
          pf
            frmt
            "Classify this counterexample: %a (P/N/U)"
            (box (Pretty.pp_ctex ~ctx))
            ctex));
    match Stdio.In_channel.input_line Stdio.stdin with
    | Some "N" ->
      { ctex with ctex_stat = add_cause ctex.ctex_stat ViolatesTargetRequires }
    | Some "P" -> { ctex with ctex_stat = Valid }
    | _ -> ctex
  in
  List.map ~f ctexs
;;

let set_term_lemma
    ~(ctx : env)
    ~(p : PsiDef.t)
    (ts : lemmas)
    ~(key : term * term option)
    ~(lemma : term)
    : unit
  =
  let term_key, splitter_key = key in
  match Hashtbl.find ts term_key with
  | None ->
    Hashtbl.add_multi
      ts
      ~key:term_key
      ~data:{ (make_term_info ~ctx ~p (fst key)) with lemmas = [ lemma ] }
  | Some term_infos ->
    let repl = ref false in
    let nl =
      List.map
        ~f:(fun ti ->
          if Option.equal Terms.equal ti.splitter splitter_key
          then (
            repl := true;
            { ti with lemmas = [ lemma ] })
          else ti)
        term_infos
    in
    if !repl
    then Hashtbl.set ts ~key:term_key ~data:nl
    else (
      let new_elt = { (make_term_info ~ctx ~p (fst key)) with lemmas = [ lemma ] } in
      Hashtbl.set ts ~key:term_key ~data:(new_elt :: nl))
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
        match ctx >- Lemmas.get_precise_lemma ~p lstate.lemmas ~key:(t, None) with
        | None -> pred_term x
        | Some inv -> mk_bin Binop.And inv (pred_term x)
      in
      (match smtterm with
      | None -> ()
      | Some x -> set_term_lemma ~ctx ~p lstate.lemmas ~key:(t, None) ~lemma:(term x))
  in
  Set.iter ~f lstate.t_set
;;

let parse_interactive_positive_example (det : term_info) (input : string) : ctex option =
  Some
    { (placeholder_ctex det) with
      ctex_model =
        List.fold
          ~init:VarMap.empty
          ~f:(fun acc s_ ->
            let s = Str.split (Str.regexp " *= *") s_ in
            if not (equal (List.length s) 2)
            then acc
            else (
              let key = trim (List.nth_exn s 0) in
              let data = mk_const (CInt (Int.of_string (trim (List.nth_exn s 1)))) in
              match VarSet.find_by_name (VarSet.of_list det.scalar_vars) key with
              | None -> acc
              | Some var -> Map.set ~data acc ~key:var))
          (Str.split (Str.regexp " *, *") input)
    }
;;

let interactive_get_positive_examples ~(ctx : Context.t) (det : term_info) =
  let vars =
    Set.filter
      ~f:(fun var ->
        match Variable.vtype ctx var with
        | None -> false
        | Some t -> not (RType.is_recursive ctx.types t))
      (Analysis.free_variables ~ctx det.term)
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
  | Some s ->
    (match parse_interactive_positive_example det s with
    | None -> []
    | Some ctex -> [ ctex ])
;;

let interactive_check_lemma ~ctx lemma_refinement_loop name vars lemma_term det =
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
          det.term
          (pp_subs ctx)
          det.recurs_elim));
  match Stdio.In_channel.input_line Stdio.stdin with
  | Some "Y" ->
    let lemma =
      match det.current_preconds with
      | None -> lemma_term
      | Some pre -> mk_bin Binop.Or (mk_un Unop.Not pre) lemma_term
    in
    Some { det with lemma_candidate = None; lemmas = lemma :: det.lemmas }
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
        { det with
          positive_ctexs = det.positive_ctexs @ interactive_get_positive_examples ~ctx det
        }
    | _ -> None)
;;
