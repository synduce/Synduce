open Lwt
open AState
open Base
open Counterexamples
open Lang
open Lang.Term
open Syguslib.Sygus
open SygusInterface
open Utils
open Smtlib
module Smt = SmtInterface
module S = Smtlib.SmtLib
module T = Term

let make_new_recurs_elim_from_term (term : term) ~(p : psi_def) : (term * term) list * variable list
    =
  Set.fold ~init:([], p.psi_reference.pargs)
    ~f:(fun (rec_elim, vars) var ->
      match Variable.vtype var with
      | None -> (rec_elim, var :: vars)
      | Some t ->
          if RType.is_recursive t then
            match Expand.mk_recursion_elimination_term p with
            | None -> (rec_elim, vars)
            | Some (a, _) ->
                (* When are a and b different? *)
                ((mk_var var, a) :: rec_elim, vars @ Set.elements (Analysis.free_variables a))
          else (rec_elim, var :: vars))
    (Analysis.free_variables term)

let make_term_state_detail ~(p : psi_def) (term : term) : term_state_detail =
  let recurs_elim, scalar_vars = make_new_recurs_elim_from_term ~p term in
  {
    term;
    lemmas = [];
    lemma_candidate = None;
    negative_ctexs = [];
    positive_ctexs = [];
    recurs_elim;
    scalar_vars;
    current_preconds = None;
  }

let flatten_rec_elim_tuples elim =
  List.concat_map
    ~f:(fun (a, b) ->
      match b.tkind with
      | TTup comps -> List.mapi comps ~f:(fun i t -> (mk_sel a i, t))
      | _ -> [ (a, b) ])
    elim

let subs_from_elim_to_elim elim1 elim2 : (term * term) list =
  List.concat_map
    ~f:(fun (a, b) ->
      let rec f lst =
        match lst with
        | [] ->
            Log.debug_msg
              (Fmt.str "Failed to get subs from ctex to term elims when matching %a." pp_term a);
            []
        | (a', b') :: tl -> if Terms.(equal a a') then [ (b', b) ] else f tl
      in
      f (flatten_rec_elim_tuples elim2))
    (flatten_rec_elim_tuples elim1)

let make_term_state_detail_from_ctex ~(p : psi_def) (is_pos_ctex : bool) (ctex : ctex) :
    term_state_detail =
  let neg = if is_pos_ctex then [] else [ ctex ] in
  let pos = if is_pos_ctex then [ ctex ] else [] in
  let recurs_elim_, scalar_vars_ = make_new_recurs_elim_from_term ~p ctex.ctex_eqn.eterm in
  let recurs_elim =
    List.filter
      ~f:(fun (l, _) ->
        let rec f l lst =
          match lst with
          | [] -> false
          | (hd, _) :: tl -> if Terms.(equal l hd) then true else f l tl
        in
        f l ctex.ctex_eqn.eelim)
      recurs_elim_
  in
  let subs_ = subs_from_elim_to_elim recurs_elim ctex.ctex_eqn.eelim in
  let scalar_vars =
    (* Filter out the vars that are not relevant to this ctex's model *)
    List.filter
      ~f:(fun v ->
        let rec g v' s =
          match s with
          | [] -> false
          | (_, b) :: tl -> if Terms.(equal (mk_var v') b) then true else g v' tl
        in
        let rec f lst =
          match lst with
          | [] -> false
          | hd :: tl -> if Variable.(equal v hd) then true else if g v subs_ then true else f tl
        in
        f (Set.elements ctex.ctex_vars))
      scalar_vars_
  in
  let subs =
    (* Filter out the vars that are not relevant to this ctex's model *)
    List.filter
      ~f:(fun (_, b) ->
        let is_in = List.mem (List.map ~f:mk_var scalar_vars) ~equal:Terms.equal b in
        is_in)
      subs_
  in
  {
    term = ctex.ctex_eqn.eterm;
    lemmas = [];
    lemma_candidate = None;
    negative_ctexs = neg;
    positive_ctexs = pos;
    recurs_elim;
    scalar_vars;
    current_preconds =
      (match ctex.ctex_eqn.eprecond with None -> None | Some pre -> Some (substitution subs pre));
  }

let mk_f_compose_r_orig ~(p : psi_def) (t : term) : term =
  let repr_of_v = if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pvar [ t ] in
  mk_app_v p.psi_reference.pvar (List.map ~f:mk_var p.psi_reference.pargs @ [ repr_of_v ])

let mk_f_compose_r_main ~(p : psi_def) (t : term) : term =
  let repr_of_v = if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pmain_symb [ t ] in
  mk_app_v p.psi_reference.pmain_symb [ repr_of_v ]

let term_detail_to_lemma ~(p : psi_def) (det : term_state_detail) : term option =
  let subst =
    List.concat_map
      ~f:(fun (t1, t2) ->
        let frt1 = mk_f_compose_r_main ~p t1 in
        match t2.tkind with
        | TTup t2s -> List.mapi t2s ~f:(fun i t2_i -> (t2_i, mk_sel frt1 i))
        | _ -> [ (t2, frt1) ])
      det.recurs_elim
  in
  let f lem = Term.substitution subst lem in
  T.mk_assoc Binop.And (List.map ~f det.lemmas)

let empty_term_state : term_state = Map.empty (module Terms)

let set_term_lemma ~(p : psi_def) (ts : term_state) ~(key : term) ~(lemma : term) : term_state =
  match Map.find ts key with
  | None -> Map.add_exn ts ~key ~data:{ (make_term_state_detail ~p key) with lemmas = [ lemma ] }
  | Some det -> Map.add_exn ts ~key ~data:{ det with lemmas = [ lemma ] }

let create_or_update_term_state_for_ctex ~(p : psi_def) (is_pos_ctex : bool) (ts : term_state)
    (ctex : ctex) : term_state =
  match Map.find ts ctex.ctex_eqn.eterm with
  | None ->
      Log.debug_msg Fmt.(str "Creating new term state for term %a" pp_term ctex.ctex_eqn.eterm);
      Map.add_exn ~key:ctex.ctex_eqn.eterm
        ~data:(make_term_state_detail_from_ctex ~p is_pos_ctex ctex)
        ts
  | Some _ ->
      Map.update ts ctex.ctex_eqn.eterm ~f:(fun maybe_det ->
          match maybe_det with
          | None -> failwith "Term detail does not exist."
          | Some det ->
              if is_pos_ctex then { det with positive_ctexs = ctex :: det.positive_ctexs }
              else
                {
                  det with
                  current_preconds =
                    (match ctex.ctex_eqn.eprecond with
                    | None -> None
                    | Some pre ->
                        let pre' =
                          substitution
                            (subs_from_elim_to_elim det.recurs_elim ctex.ctex_eqn.eelim)
                            pre
                        in
                        Some pre');
                  negative_ctexs = ctex :: det.negative_ctexs;
                  positive_ctexs = [];
                })

let update_term_state_for_ctexs ~(p : psi_def) (ts : term_state) ~(pos_ctexs : ctex list)
    ~(neg_ctexs : ctex list) : term_state =
  List.fold
    ~init:(List.fold ~init:ts ~f:(create_or_update_term_state_for_ctex ~p true) pos_ctexs)
    ~f:(create_or_update_term_state_for_ctex ~p false)
    neg_ctexs

let get_lemma ~(p : psi_def) (ts : term_state) ~(key : term) : term option =
  match Map.find ts key with None -> None | Some det -> term_detail_to_lemma ~p det

let get_term_state_detail (ts : term_state) ~(key : term) = Map.find ts key

let add_lemmas_interactively ~(p : psi_def) (lstate : refinement_loop_state) : refinement_loop_state
    =
  let env_in_p = VarSet.of_list p.psi_reference.pargs in
  let f existing_lemmas t =
    let vars = Set.union (Analysis.free_variables t) env_in_p in
    let env = VarSet.to_env vars in

    Log.info (fun frmt () -> Fmt.pf frmt "Please provide a constraint for \"@[%a@]\"." pp_term t);
    Log.verbose (fun frmt () ->
        Fmt.pf frmt "Environment:@;@[functions %s, %s and %s@]@;and @[%a@]."
          p.psi_reference.pvar.vname p.psi_target.pvar.vname p.psi_repr.pvar.vname VarSet.pp vars);
    match Stdio.In_channel.input_line Stdio.stdin with
    | None | Some "" ->
        Log.info (fun frmt () -> Fmt.pf frmt "No additional constraint provided.");
        existing_lemmas
    | Some x -> (
        let smtterm =
          try
            let sexpr = Sexplib.Sexp.of_string x in
            Smtlib.SmtLib.smtTerm_of_sexp sexpr
          with Failure _ -> None
        in
        let pred_term = SmtInterface.term_of_smt env in
        let term x =
          match get_lemma ~p existing_lemmas ~key:t with
          | None -> pred_term x
          | Some inv -> mk_bin Binop.And inv (pred_term x)
        in
        match smtterm with
        | None -> existing_lemmas
        | Some x -> set_term_lemma ~p existing_lemmas ~key:t ~lemma:(term x))
  in
  { lstate with term_state = Set.fold ~f ~init:lstate.term_state lstate.t_set }

let ith_synth_fun index = "lemma_" ^ Int.to_string index

let synthfun_of_ctex ~(p : psi_def) (det : term_state_detail) (lem_id : int) :
    command * (string * sygus_sort) list * string =
  let params =
    List.map
      ~f:(fun scalar -> (scalar.vname, sort_of_rtype (Variable.vtype_or_new scalar)))
      det.scalar_vars
  in
  let ret_sort = sort_of_rtype RType.TBool in
  let opset =
    List.fold ~init:OpSet.empty
      ~f:(fun acc func -> Set.union acc (Analysis.operators_of func.f_body))
      (PMRS.func_of_pmrs p.psi_reference @ PMRS.func_of_pmrs p.psi_repr
      @ match p.psi_tinv with None -> [] | Some pmrs -> PMRS.func_of_pmrs pmrs)
  in
  (* OpSet.of_list [ Binary Binop.Mod ] in *)
  let grammar = Grammars.generate_grammar ~guess:None ~bools:true opset params ret_sort in
  let logic = logic_of_operators opset in
  (CSynthFun (ith_synth_fun lem_id, params, ret_sort, grammar), params, logic)

let term_var_string term : string =
  match Set.elements (Analysis.free_variables term) with
  | [] -> failwith (Fmt.str "Failed to extract string of variable name in term %a" pp_term term)
  | var :: _ -> var.vname

let convert_term_rec_to_ctex_rec ~(p : psi_def) (det : term_state_detail) (ctex : ctex)
    (name : string) : string =
  let rec g recvar lst =
    match lst with
    | [] -> failwith (Fmt.str "Could not find name %s in the ctex's recursion elimination." name)
    | (a, b) :: tl ->
        let i = term_var_string b in
        let l = term_var_string a in
        if String.(equal l recvar) then i else g recvar tl
  in
  let rec h recvar n elim =
    match elim with
    | [] -> failwith (Fmt.str "Could not find ctex rec elim entry for tuple entry named %s." name)
    | (a, b) :: tl ->
        let l = term_var_string a in
        if String.(equal l recvar) then
          match b.tkind with
          | TTup vars -> term_var_string (List.nth_exn vars n)
          | _ ->
              failwith
                Fmt.(str "Cannot get tuple entry %s in ctex rec elim; %s is not a tuple.)" name l)
        else h recvar n tl
  in
  let rec f lst =
    match lst with
    | [] -> failwith (Fmt.str "Could not find name %s in this term's recursion elimination." name)
    | (a, b) :: tl -> (
        match b.tkind with
        | TTup vars -> (
            match
              List.find_mapi vars ~f:(fun n x ->
                  let l = term_var_string a in
                  let i = term_var_string x in
                  if String.(equal i name) then Some (h l n ctex.ctex_eqn.eelim) else None)
            with
            | Some s -> s
            | None -> f tl)
        | _ ->
            let i = term_var_string b in
            let l = term_var_string a in
            if String.(equal i name) then g l ctex.ctex_eqn.eelim else f tl)
  in
  match
    VarSet.find_by_name
      (Set.union
         (Analysis.free_variables det.term)
         (Set.union (VarSet.of_list p.psi_reference.pargs) ctex.ctex_vars))
      name
  with
  | None -> f det.recurs_elim
  | Some _ -> name

let ctex_model_to_args ~(p : psi_def) (det : term_state_detail)
    (params : (string * sygus_sort) list) ctex : sygus_term list =
  List.map
    ~f:(fun (name_, _) ->
      match
        let name = convert_term_rec_to_ctex_rec ~p det ctex name_ in
        Map.find ctex.ctex_model
          (match
             VarSet.find_by_name
               (Set.union
                  (* Don't include functions, we won't get a model for them in CVC5. *)
                  (Analysis.free_variables ~include_functions:false det.term)
                  (Set.union (VarSet.of_list p.psi_reference.pargs) ctex.ctex_vars))
               name
           with
          | None ->
              failwith
                (Fmt.str "Failed to extract argument list from ctex model (%s unknown)." name)
          | Some v -> v)
      with
      | None -> failwith "Failed to extract argument list from ctex model."
      | Some t -> sygus_of_term t)
    params

let constraint_of_neg_ctex index ~(p : psi_def) (det : term_state_detail)
    (params : (string * sygus_sort) list) ctex =
  CConstraint
    (SyApp
       ( IdSimple "not",
         [ SyApp (IdSimple (ith_synth_fun index), ctex_model_to_args ~p det params ctex) ] ))

let constraint_of_pos_ctex index ~(p : psi_def) (det : term_state_detail)
    (params : (string * sygus_sort) list) ctex =
  CConstraint (SyApp (IdSimple (ith_synth_fun index), ctex_model_to_args ~p det params ctex))

let log_soln s vs t =
  Log.verbose (fun frmt () ->
      Fmt.pf frmt "Lemma candidate: \"%s %s = @[%a@]\"." s
        (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vs))
        pp_term t)

let handle_lemma_synth_response (det : term_state_detail) (resp : solver_response option) =
  let parse_synth_fun (fname, _fargs, _, fbody) =
    let body, _ =
      infer_type (term_of_sygus (VarSet.to_env (VarSet.of_list det.scalar_vars)) fbody)
    in
    (fname, det.scalar_vars, body)
  in
  match resp with
  | Some (RSuccess resps) ->
      let soln = List.map ~f:parse_synth_fun resps in
      let _ = List.iter ~f:(fun (s, vs, t) -> log_soln s vs t) soln in
      Some soln
  | Some RInfeasible | Some RFail | Some RUnknown | None -> None

let smt_of_recurs_elim_eqns (elim : (term * term) list) ~(p : psi_def) : S.smtTerm =
  let lst =
    List.map
      ~f:(fun (t1, t2) ->
        S.mk_eq (Smt.smt_of_term (mk_f_compose_r_orig ~p t1)) (Smt.smt_of_term t2))
      elim
  in
  if equal (List.length lst) 0 then S.mk_true else S.mk_assoc_and lst

let smt_of_aux_ensures ~(p : psi_def) : S.smtTerm list =
  let mk_sort maybe_rtype =
    match maybe_rtype with None -> S.mk_int_sort | Some rtype -> Smt.sort_of_rtype rtype
  in
  let pmrss : PMRS.t list =
    [ p.psi_reference; p.psi_target; p.psi_reference ]
    @ match p.psi_tinv with None -> [] | Some tinv -> [ tinv ]
  in
  let vars : variable list =
    List.concat
      (List.map
         ~f:(fun (pmrs : PMRS.t) ->
           List.filter
             ~f:(fun v -> (not Variable.(v = pmrs.pmain_symb)) && not Variable.(v = pmrs.pvar))
             (Set.elements pmrs.pnon_terminals))
         pmrss)
  in
  List.fold ~init:[]
    ~f:(fun acc v ->
      let maybe_ens = Specifications.get_ensures v in
      match maybe_ens with
      | None -> acc
      | Some t ->
          let arg_types = fst (RType.fun_typ_unpack (Variable.vtype_or_new v)) in
          let arg_vs = List.map ~f:(fun t -> Variable.mk ~t:(Some t) (Alpha.fresh ())) arg_types in
          let args = List.map ~f:mk_var arg_vs in
          let quants =
            List.map ~f:(fun var -> (S.SSimple var.vname, mk_sort (Variable.vtype var))) arg_vs
          in
          let ens = Reduce.reduce_term (mk_app t [ mk_app_v v args ]) in
          let smt = S.mk_forall quants (Smt.smt_of_term ens) in
          smt :: acc)
    vars

let smt_of_tinv_app ~(p : psi_def) (det : term_state_detail) =
  match p.psi_tinv with
  | None -> failwith "No TInv has been specified. Cannot make smt of tinv app."
  | Some pmrs -> S.mk_simple_app pmrs.pvar.vname [ Smt.smt_of_term det.term ]

let smt_of_lemma_app (lemma_name, lemma_args, _lemma_body) =
  S.mk_simple_app lemma_name (List.map ~f:(fun var -> S.mk_var var.vname) lemma_args)

let smt_of_lemma_validity ~(p : psi_def) lemma (det : term_state_detail) =
  let mk_sort maybe_rtype =
    match maybe_rtype with None -> S.mk_int_sort | Some rtype -> Smt.sort_of_rtype rtype
  in
  let quants =
    List.map
      ~f:(fun var -> (S.SSimple var.vname, mk_sort (Variable.vtype var)))
      (Set.elements (Analysis.free_variables det.term)
      @ p.psi_reference.pargs
      @ List.concat_map ~f:(fun (_, b) -> Set.elements (Analysis.free_variables b)) det.recurs_elim
      )
  in
  let preconds =
    match det.current_preconds with None -> [] | Some pre -> [ Smt.smt_of_term pre ]
  in
  let if_condition =
    S.mk_assoc_and
      ([ smt_of_tinv_app ~p det; smt_of_recurs_elim_eqns det.recurs_elim ~p ] @ preconds)
  in
  let if_then = smt_of_lemma_app lemma in
  [ S.mk_assert (S.mk_not (S.mk_forall quants (S.mk_or (S.mk_not if_condition) if_then))) ]

let set_up_lemma_solver solver ~(p : psi_def) lemma_candidate =
  let%lwt () = Smt.AsyncSmt.set_logic solver "ALL" in
  let%lwt () = Smt.AsyncSmt.set_option solver "quant-ind" "true" in
  let%lwt () = Smt.AsyncSmt.set_option solver "produce-models" "true" in
  let%lwt () = Smt.AsyncSmt.set_option solver "incremental" "true" in
  let%lwt () =
    if !Config.induction_proof_tlimit >= 0 then
      Smt.AsyncSmt.set_option solver "tlimit" (Int.to_string !Config.induction_proof_tlimit)
    else return ()
  in
  let%lwt () = Smt.AsyncSmt.load_min_max_defs solver in
  let%lwt () =
    Lwt_list.iter_p
      (fun x ->
        let%lwt _ = Smt.AsyncSmt.exec_command solver x in
        return ())
      ((match p.psi_tinv with None -> [] | Some tinv -> Smt.smt_of_pmrs tinv)
      @ (if p.psi_repr_is_identity then Smt.smt_of_pmrs p.psi_reference
        else Smt.smt_of_pmrs p.psi_reference @ Smt.smt_of_pmrs p.psi_repr)
      (* Assert invariants on functions *)
      @ List.map ~f:S.mk_assert (smt_of_aux_ensures ~p)
      (* Declare lemmas. *)
      @ [
          (match lemma_candidate with
          | name, vars, body ->
              Smt.mk_def_fun_command name
                (List.map ~f:(fun v -> (v.vname, Variable.vtype_or_new v)) vars)
                RType.TBool body);
        ])
  in
  return ()

let set_up_bounded_solver (logic : string) (vars : VarSet.t) solver =
  let%lwt () = SmtInterface.AsyncSmt.set_logic solver logic in
  let%lwt () = SmtInterface.AsyncSmt.set_option solver "produce-models" "true" in
  let%lwt () = SmtInterface.AsyncSmt.set_option solver "incremental" "true" in
  let%lwt () =
    if !Config.induction_proof_tlimit >= 0 then
      SmtInterface.AsyncSmt.set_option solver "tlimit"
        (Int.to_string !Config.induction_proof_tlimit)
    else return ()
  in
  let%lwt () = SmtInterface.AsyncSmt.load_min_max_defs solver in
  let%lwt () = SmtInterface.AsyncSmt.declare_all solver (SmtInterface.decls_of_vars vars) in
  return ()

let classify_ctexs_opt ~(p : psi_def) ctexs : ctex list =
  if !Config.classify_ctex then
    let f ctex =
      Log.info (fun frmt () ->
          Fmt.(pf frmt "Classify this counterexample: %a (P/N/U)" (box pp_ctex) ctex));
      match Stdio.In_channel.input_line Stdio.stdin with
      | Some "N" -> { ctex with ctex_stat = Spurious ViolatesTargetRequires }
      | Some "P" -> { ctex with ctex_stat = Valid }
      | _ -> ctex
    in
    List.map ~f ctexs
  else classify_ctexs ~p ctexs

let smt_of_disallow_ctex_values (det : term_state_detail) : S.smtTerm =
  let ctexs = det.positive_ctexs in
  S.mk_assoc_and
    (List.map
       ~f:(fun ctex ->
         S.mk_not
           (S.mk_assoc_and
              (Map.fold
                 ~f:(fun ~key ~data acc ->
                   let var =
                     let subs = subs_from_elim_to_elim det.recurs_elim ctex.ctex_eqn.eelim in
                     Term.substitution subs (mk_var key)
                   in
                   S.mk_eq (Smt.smt_of_term var) (Smt.smt_of_term data) :: acc)
                 ~init:[] ctex.ctex_model)))
       ctexs)

let set_up_to_get_model solver ~(p : psi_def) lemma (det : term_state_detail) =
  (* Step 1. Declare vars for term, and assert that term satisfies tinv. *)
  let%lwt () =
    Smt.AsyncSmt.declare_all solver (Smt.decls_of_vars (Analysis.free_variables det.term))
  in
  let%lwt _ = Smt.AsyncSmt.exec_command solver (S.mk_assert (smt_of_tinv_app ~p det)) in
  (* Step 2. Declare scalars (vars for recursion elimination & spec param) and their constraints (preconds & recurs elim eqns) *)
  let%lwt () =
    Smt.AsyncSmt.declare_all solver (Smt.decls_of_vars (VarSet.of_list det.scalar_vars))
  in
  let%lwt _ =
    Smt.AsyncSmt.exec_command solver (S.mk_assert (smt_of_recurs_elim_eqns det.recurs_elim ~p))
  in
  let%lwt () =
    match det.current_preconds with
    | None -> return ()
    | Some pre ->
        let%lwt _ = Smt.AsyncSmt.exec_command solver (S.mk_assert (Smt.smt_of_term pre)) in
        return ()
  in
  (* Step 3. Disallow repeated positive examples. *)
  let%lwt () =
    if List.length det.positive_ctexs > 0 then
      let%lwt _ =
        Smt.AsyncSmt.exec_command solver (S.mk_assert (smt_of_disallow_ctex_values det))
      in
      return ()
    else return ()
  in
  (* Step 4. Assert that lemma candidate is false. *)
  Smt.AsyncSmt.exec_command solver (S.mk_assert (S.mk_not (smt_of_lemma_app lemma)))

let mk_model_sat_asserts det f_o_r instantiate =
  let f v =
    let v_val = mk_var (List.find_exn ~f:(fun x -> equal v.vid x.vid) det.scalar_vars) in
    match List.find_map ~f:(find_original_var_and_proj v) det.recurs_elim with
    | Some (original_recursion_var, proj) -> (
        match original_recursion_var.tkind with
        | TVar ov when Option.is_some (instantiate ov) ->
            let instantiation = Option.value_exn (instantiate ov) in
            if proj >= 0 then
              let t = Reduce.reduce_term (mk_sel (f_o_r instantiation) proj) in
              Smt.smt_of_term (mk_bin Binop.Eq t v_val)
            else
              let t = f_o_r instantiation in
              Smt.smt_of_term (mk_bin Binop.Eq t v_val)
        | _ ->
            Log.error_msg
              Fmt.(str "Warning: skipped instantiating %a." pp_term original_recursion_var);
            SmtLib.mk_true)
    | None -> Smt.smt_of_term (mk_bin Binop.Eq (mk_var v) v_val)
  in
  List.map ~f det.scalar_vars

let verify_lemma_bounded ~(p : psi_def) (det : term_state_detail) lemma_candidate :
    Smt.AsyncSmt.response * int Lwt.u =
  let task (solver, starter) =
    let%lwt _ = starter in
    let%lwt _ = set_up_bounded_solver "LIA" (VarSet.of_list det.scalar_vars) solver in
    let steps = ref 0 in
    let rec check_bounded_sol accum terms =
      let f accum t =
        let rec_instantation =
          Option.value ~default:VarMap.empty (Analysis.matches t ~pattern:det.term)
        in
        let%lwt _ = accum in
        let f_compose_r t =
          let repr_of_v = if p.psi_repr_is_identity then t else Reduce.reduce_pmrs p.psi_repr t in
          Reduce.reduce_term (Reduce.reduce_pmrs p.psi_reference repr_of_v)
        in
        let subs =
          List.map
            ~f:(fun (orig_rec_var, elimv) ->
              match orig_rec_var.tkind with
              | TVar rec_var when Map.mem rec_instantation rec_var ->
                  (elimv, f_compose_r (Map.find_exn rec_instantation rec_var))
              | _ -> failwith "all elimination variables should be substituted.")
            det.recurs_elim
          (* Map.fold ~init:[] ~f:(fun ~key ~data acc -> (mk_var key, data) :: acc) rec_instantation *)
        in
        let preconds =
          Option.to_list (Option.map ~f:(fun t -> substitution subs t) det.current_preconds)
        in
        let model_sat = mk_model_sat_asserts det f_compose_r (Map.find rec_instantation) in
        let%lwt () = Smt.AsyncSmt.spush solver in
        (* Assert that preconditions hold and map original term's recurs elim variables to the variables that they reduce to for this concrete term. *)
        let%lwt _ =
          Smt.AsyncSmt.declare_all solver
            (Smt.decls_of_vars
               (List.fold ~init:VarSet.empty
                  ~f:(fun acc t -> Set.union acc (Analysis.free_variables t))
                  (preconds @ Map.data rec_instantation)))
        in
        let%lwt () =
          Smt.AsyncSmt.smt_assert solver
            (SmtLib.mk_assoc_and (List.map ~f:Smt.smt_of_term preconds @ model_sat))
        in
        (* Assert that TInv is true for this concrete term t *)
        let%lwt _ =
          match p.psi_tinv with
          | None -> return ()
          | Some tinv ->
              let tinv_t = Reduce.reduce_pmrs tinv t in
              let%lwt _ =
                Smt.AsyncSmt.declare_all solver (Smt.decls_of_vars (Analysis.free_variables tinv_t))
              in
              let%lwt _ = Smt.AsyncSmt.smt_assert solver (Smt.smt_of_term tinv_t) in

              return ()
        in
        (* Assert that lemma is false for this concrete term t  *)
        let _, _, lemma = lemma_candidate in
        let%lwt _ =
          Smt.AsyncSmt.exec_command solver
            (S.mk_assert (S.mk_not (Smt.smt_of_term (substitution subs lemma))))
        in
        let%lwt resp = Smt.AsyncSmt.check_sat solver in
        (* Note that I am getting a model after check-sat unknown response. This may not halt.  *)
        let%lwt result =
          match resp with
          | SmtLib.Sat | SmtLib.Unknown ->
              let%lwt model = Smt.AsyncSmt.get_model solver in
              return (Some model)
          | _ -> return None
        in
        let%lwt () = Smt.AsyncSmt.spop solver in
        return (resp, result)
      in
      match terms with
      | [] -> accum
      | t0 :: tl -> (
          let%lwt accum' = f accum t0 in
          match accum' with
          | status, Some model -> return (status, Some model)
          | _ -> check_bounded_sol (return accum') tl)
    in
    let rec expand_loop u =
      match (Set.min_elt u, !steps < !Config.num_expansions_check) with
      | Some t0, true -> (
          let tset, u' = Expand.simple t0 in
          let%lwt check_result =
            check_bounded_sol (return (SmtLib.Unknown, None)) (Set.elements tset)
          in
          steps := !steps + Set.length tset;
          match check_result with
          | _, Some model ->
              Log.debug_msg
                "Bounded lemma verification has found a counterexample to the lemma candidate.";
              return model
          | _ -> expand_loop (Set.union (Set.remove u t0) u'))
      | None, true ->
          (* All expansions have been checked. *)
          return SmtLib.Unsat
      | _, false ->
          (* Check reached limit. *)
          Log.debug_msg "Bounded lemma verification has reached limit.";
          if !Config.bounded_lemma_check then return SmtLib.Unsat else return SmtLib.Unknown
    in
    let%lwt res = expand_loop (TermSet.singleton det.term) in
    return res
  in
  Smt.AsyncSmt.(cancellable_task (make_cvc_solver ()) task)

let verify_lemma_unbounded ~(p : psi_def) (det : term_state_detail) lemma_candidate :
    Smt.AsyncSmt.response * int Lwt.u =
  let build_task (cvc4_instance, task_start) =
    let%lwt _ = task_start in
    let%lwt () = set_up_lemma_solver cvc4_instance ~p lemma_candidate in
    let%lwt () =
      (Lwt_list.iter_p (fun x ->
           let%lwt _ = Smt.AsyncSmt.exec_command cvc4_instance x in
           return ()))
        (smt_of_lemma_validity ~p lemma_candidate det)
    in
    let%lwt resp = Smt.AsyncSmt.check_sat cvc4_instance in
    let%lwt final_response =
      match resp with
      | Sat | Unknown -> (
          let%lwt _ = set_up_to_get_model cvc4_instance ~p lemma_candidate det in
          let%lwt resp' = Smt.AsyncSmt.check_sat cvc4_instance in
          match resp' with
          | Sat | Unknown -> Smt.AsyncSmt.get_model cvc4_instance
          | _ -> return resp')
      | _ -> return resp
    in
    let%lwt () = Smt.AsyncSmt.close_solver cvc4_instance in
    Log.debug_msg "Unbounded lemma verification is complete.";
    return final_response
  in
  Smt.AsyncSmt.(cancellable_task (Smt.AsyncSmt.make_cvc_solver ()) build_task)

let verify_lemma_candidate ~(p : psi_def) (det : term_state_detail) : Smt.SyncSmt.solver_response =
  match det.lemma_candidate with
  | None -> failwith "Cannot verify lemma candidate; there is none."
  | Some lemma_candidate ->
      Log.verbose (fun f () -> Fmt.(pf f "Checking lemma candidate..."));
      let resp =
        try
          Lwt_main.run
            (let pr1, resolver1 = verify_lemma_bounded ~p det lemma_candidate in
             let pr2, resolver2 = verify_lemma_unbounded ~p det lemma_candidate in
             Lwt.wakeup resolver2 1;
             Lwt.wakeup resolver1 1;
             (* The first call to return is kept, the other one is ignored. *)
             Lwt.pick [ pr1; pr2 ])
        with End_of_file ->
          Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
          Log.error_msg "Please inspect logs.";
          SmtLib.Unknown
      in
      resp

let placeholder_ctex (det : term_state_detail) : ctex =
  {
    ctex_eqn =
      {
        eterm = det.term;
        eprecond = det.current_preconds;
        eelim = det.recurs_elim;
        (* Placeholder values for elhs, erhs, these don't matter for us *)
        elhs = det.term;
        erhs = det.term;
      };
    ctex_vars = VarSet.of_list det.scalar_vars;
    ctex_model = VarMap.empty;
    ctex_stat = Unknown;
  }

let parse_positive_example_solver_model response (det : term_state_detail) =
  match response with
  | SmtLib.SExps s ->
      let model = Smt.model_to_constmap (SExps s) in
      let m, _ =
        Map.partitioni_tf
          ~f:(fun ~key ~data:_ ->
            Option.is_some (VarSet.find_by_name (VarSet.of_list det.scalar_vars) key))
          model
      in
      (* Remap the names to ids of the original variables in m' *)
      [
        ({
           (placeholder_ctex det) with
           ctex_model =
             Map.fold ~init:VarMap.empty
               ~f:(fun ~key ~data acc ->
                 match VarSet.find_by_name (VarSet.of_list det.scalar_vars) key with
                 | None ->
                     Log.info (fun f () -> Fmt.(pf f "Could not find by name %s" key));
                     acc
                 | Some var -> Map.set ~data acc ~key:var)
               m;
         }
          : ctex);
      ]
  | _ -> failwith "Parse model failure: Positive example cannot be found during lemma refinement."

let trim (s : string) = Str.global_replace (Str.regexp "[\r\n\t ]") "" s

let parse_interactive_positive_example (det : term_state_detail) (input : string) : ctex option =
  Some
    {
      (placeholder_ctex det) with
      ctex_model =
        List.fold ~init:VarMap.empty
          ~f:(fun acc s_ ->
            let s = Str.split (Str.regexp " *= *") s_ in
            if not (equal (List.length s) 2) then acc
            else
              let key = trim (List.nth_exn s 0) in
              let data = mk_const (CInt (Int.of_string (trim (List.nth_exn s 1)))) in
              match VarSet.find_by_name (VarSet.of_list det.scalar_vars) key with
              | None -> acc
              | Some var -> Map.set ~data acc ~key:var)
          (Str.split (Str.regexp " *, *") input);
    }

let interactive_get_positive_examples (det : term_state_detail) =
  let vars =
    Set.filter
      ~f:(fun var ->
        match Variable.vtype var with None -> false | Some t -> not (RType.is_recursive t))
      (Analysis.free_variables det.term)
  in
  Log.info (fun f () ->
      Fmt.(
        pf f "Enter an example as \"%s\""
          (String.concat ~sep:", "
             (List.map
                ~f:(fun var ->
                  var.vname ^ "=<"
                  ^ (match Variable.vtype var with
                    | None -> ""
                    | Some t -> ( match RType.base_name t with None -> "" | Some tname -> tname))
                  ^ ">")
                (Set.elements vars)))));
  match Stdio.In_channel.input_line Stdio.stdin with
  | None -> []
  | Some s -> (
      match parse_interactive_positive_example det s with None -> [] | Some ctex -> [ ctex ])

let synthesize_new_lemma ~(p : psi_def) (det : term_state_detail) :
    (string * variable list * term) option =
  Log.debug (fun f () ->
      Fmt.(
        match det.current_preconds with
        | None ->
            pf f "Synthesizing a new lemma candidate for term %a[%a]." pp_term det.term pp_subs
              det.recurs_elim
        | Some pre ->
            pf f "Synthesizing a new lemma candidate for term %a[%a] with precondition %a" pp_term
              det.term pp_subs det.recurs_elim pp_term pre));
  let lem_id = 0 in
  let synth_objs, params, logic = synthfun_of_ctex ~p det lem_id in
  let neg_constraints =
    List.map ~f:(constraint_of_neg_ctex lem_id ~p det params) det.negative_ctexs
  in
  let pos_constraints =
    List.map ~f:(constraint_of_pos_ctex lem_id ~p det params) det.positive_ctexs
  in
  let extra_defs = [ max_definition; min_definition ] in
  let commands =
    CSetLogic logic
    :: (extra_defs @ [ synth_objs ] @ neg_constraints @ pos_constraints @ [ CCheckSynth ])
  in
  match handle_lemma_synth_response det (Syguslib.Solvers.SygusSolver.solve_commands commands) with
  | None -> None
  | Some solns -> List.nth solns 0

let rec lemma_refinement_loop (det : term_state_detail) ~(p : psi_def) : term_state_detail option =
  match synthesize_new_lemma ~p det with
  | None ->
      Log.debug_msg "Lemma synthesis failure.";
      None
  | Some (name, vars, lemma_term) -> (
      if !Config.interactive_check_lemma then (
        Log.info (fun f () ->
            Fmt.(
              pf f "Is the lemma \"%s %s = @[%a@]\" for term %a[%a] correct? [Y/N]" name
                (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vars))
                pp_term lemma_term pp_term det.term pp_subs det.recurs_elim));
        match Stdio.In_channel.input_line Stdio.stdin with
        | Some "Y" ->
            let lemma =
              match det.current_preconds with
              | None -> lemma_term
              | Some pre -> mk_bin Binop.Or (mk_un Unop.Not pre) lemma_term
            in
            Some { det with lemma_candidate = None; lemmas = lemma :: det.lemmas }
        | _ -> (
            Log.info (fun f () ->
                Fmt.(
                  pf f
                    "Would you like to provide a non-spurious example in which the lemma is false? \
                     [Y/N]"));
            match Stdio.In_channel.input_line Stdio.stdin with
            | Some "Y" ->
                lemma_refinement_loop
                  {
                    det with
                    positive_ctexs = det.positive_ctexs @ interactive_get_positive_examples det;
                  }
                  ~p
            | _ -> None))
      else
        match
          verify_lemma_candidate ~p { det with lemma_candidate = Some (name, vars, lemma_term) }
        with
        | Unsat ->
            Log.verbose (fun f () -> Fmt.(pf f "This lemma has been proven correct."));
            Log.info (fun frmt () ->
                Fmt.pf frmt "Lemma for term %a: \"%s %s = @[%a@]\"." pp_term det.term name
                  (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vars))
                  pp_term lemma_term);
            let lemma =
              match det.current_preconds with
              | None -> lemma_term
              | Some pre -> mk_bin Binop.Or (mk_un Unop.Not pre) lemma_term
            in
            Some { det with lemma_candidate = None; lemmas = lemma :: det.lemmas }
        | SmtLib.SExps x ->
            Log.verbose (fun f () ->
                Fmt.(pf f "This lemma has not been proven correct. Refining lemma..."));
            let new_positive_ctexs = parse_positive_example_solver_model (SmtLib.SExps x) det in
            List.iter
              ~f:(fun ctex ->
                Log.verbose (fun f () ->
                    Fmt.(pf f "Found a positive example: %a" (box pp_ctex) ctex)))
              new_positive_ctexs;
            lemma_refinement_loop
              { det with positive_ctexs = det.positive_ctexs @ new_positive_ctexs }
              ~p
        | Sat ->
            Log.error_msg "Lemma verification returned Sat, which is weird.";
            None
        | Unknown ->
            Log.error_msg "Lemma verification returned Unknown.";
            None
        | _ ->
            Log.error_msg "Lemma verification is indeterminate.";
            None)

let ctexs_for_lemma_synt ctex =
  match ctex.ctex_stat with
  | Valid -> `Fst ctex
  | Spurious ViolatesTargetRequires -> `Snd ctex
  | _ -> `Trd ctex

let ctexs_for_ensures_synt ctex =
  match ctex.ctex_stat with
  | Valid -> `Fst ctex
  | Spurious NotInReferenceImage -> `Snd ctex
  | _ -> `Trd ctex

let add_ensures_to_term_state_detail ~(p : psi_def) (det : term_state_detail)
    (_ctex_elim : (term * term) list) (ensures : term) : term_state_detail =
  let new_ensures =
    List.map det.recurs_elim ~f:(fun (t, _) ->
        let f_compose_r t_ =
          let repr_of_v = if p.psi_repr_is_identity then t_ else Reduce.reduce_pmrs p.psi_repr t_ in
          Reduce.reduce_term (Reduce.reduce_pmrs p.psi_reference repr_of_v)
        in
        Reduce.reduce_term (mk_app ensures [ f_compose_r t ]))
  in
  { det with lemmas = new_ensures @ det.lemmas }

let add_ensures_to_term_state ~(p : psi_def) (ensures : term) (ctex_elim : (term * term) list)
    (ts : term_state) : term_state =
  Map.fold ts
    ~init:(Map.empty (module Terms))
    ~f:(fun ~key ~data:det acc ->
      let new_det = add_ensures_to_term_state_detail ~p det ctex_elim ensures in
      Map.add_exn ~key ~data:new_det acc)

let synthesize_lemmas ~(p : psi_def) synt_failure_info (lstate : refinement_loop_state) :
    (refinement_loop_state, solver_response) Result.t =
  (*
    Example: the synt_failure_info should be a list of unrealizability counterexamples, which
    are pairs of counterexamples.
    Each counterexample can be classfied as positive or negative w.r.t to the predicate p.psi_tinv.
    The lemma corresponding to a particular term should be refined to eliminate the counterexample
    (a counterexample cex is also asscociated to a particular term through cex.ctex_eqn.eterm)
   *)
  let new_state, success =
    match synt_failure_info with
    | _, Either.Second unrealizability_ctexs ->
        (* Forget about the specific association in pairs. *)
        let ctexs = List.concat_map unrealizability_ctexs ~f:(fun uc -> [ uc.ci; uc.cj ]) in
        let classified_ctexs = classify_ctexs_opt ~p ctexs in
        let ensures_positives, ensures_negatives, _ =
          List.partition3_map ~f:ctexs_for_ensures_synt classified_ctexs
        in
        if List.length ensures_negatives > 0 then (
          let maybe_pred = ImagePredicates.synthesize ~p ensures_positives ensures_negatives in
          match maybe_pred with
          | None -> (lstate.term_state, false)
          | Some ensures ->
              (match Specifications.get_ensures p.psi_reference.pvar with
              | None -> Specifications.set_ensures p.psi_reference.pvar ensures
              | Some old_ensures ->
                  let var : variable =
                    Variable.mk ~t:(Some p.psi_reference.poutput_typ) (Alpha.fresh ())
                  in
                  Specifications.set_ensures p.psi_reference.pvar
                    (mk_fun [ FPatVar var ]
                       (mk_bin Binop.And
                          (mk_app old_ensures [ mk_var var ])
                          (mk_app ensures [ mk_var var ]))));
              (lstate.term_state, true))
        else
          (* Classify in negative and positive cexs. *)
          let lemma_synt_positives, lemma_synt_negatives, _ =
            List.partition3_map ~f:ctexs_for_lemma_synt classified_ctexs
          in
          let ts : term_state =
            update_term_state_for_ctexs ~p lstate.term_state ~neg_ctexs:lemma_synt_negatives
              ~pos_ctexs:lemma_synt_positives
          in
          if List.is_empty lemma_synt_negatives then (ts, false)
          else
            Map.fold ts
              ~init:(Map.empty (module Terms), true)
              ~f:(fun ~key ~data:det (acc, status) ->
                if not status then (acc, status)
                else
                  match lemma_refinement_loop det ~p with
                  | None -> (acc, false)
                  | Some det -> (Map.add_exn ~key ~data:det acc, status))
    | _ -> failwith "There is no synt_failure_info in synthesize_lemmas."
  in
  if
    success
    || !Config.interactive_lemmas_loop
       &&
       (Log.info (fun frmt () -> Fmt.pf frmt "No luck. Try again? (Y/N)");
        match Stdio.In_channel.input_line Stdio.stdin with
        | None | Some "" | Some "N" -> false
        | Some "Y" -> true
        | _ -> false)
  then Ok { lstate with term_state = new_state }
  else
    match synt_failure_info with
    | RFail, _ ->
        Log.error_msg "SyGuS solver failed to find a solution.";
        Error RFail
    | RInfeasible, _ ->
        (* Rare - but the synthesis solver can answer "infeasible", in which case it can give
           counterexamples. *)
        Log.info
          Fmt.(
            fun frmt () ->
              pf frmt "@[<hov 2>This problem has no solution. Counterexample set:@;%a@]"
                (list ~sep:sp pp_term) (Set.elements lstate.t_set));
        Error RInfeasible
    | RUnknown, _ ->
        (* In most cases if the synthesis solver does not find a solution and terminates, it will
           answer unknowns. We interpret it as "no solution can be found". *)
        Log.error_msg "SyGuS solver returned unknown.";
        Error RUnknown
    | s_resp, _ ->
        Log.error_msg "SyGuS solver did not succeed.";
        Error s_resp
