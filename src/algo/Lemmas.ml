open Lwt
open AState
open Base
open Counterexamples
open Lang
open Lang.Term
open Lwt.Syntax
open Rewriter
open Syguslib.Sygus
open SygusInterface
open Utils
open Smtlib
open SmtInterface
module S = Smtlib.SmtLib
module T = Term

let empty_term_state : term_state = Map.empty (module Terms)

let placeholder_ctex (det : term_state_detail) : ctex =
  { ctex_eqn =
      { eterm = det.term
      ; eprecond = det.current_preconds
      ; eelim = det.recurs_elim
      ; (* Placeholder values for elhs, erhs, these don't matter for us *)
        elhs = det.term
      ; erhs = det.term
      }
  ; ctex_vars = VarSet.of_list det.scalar_vars
  ; ctex_model = VarMap.empty
  ; ctex_stat = Unknown
  }
;;

(* ============================================================================================= *)
(*                                  Creating and updating term states                            *)
(* ============================================================================================= *)

(** Given a term and a problem definition, output a recursion elimination map (a list associating some
 terms of recursive type to terms of bounded type). The second element of the pair returned by this
 function is the set of scalar variables that would appear in the term after recursion elimination has
 been applied.
*)
let recurs_elim_of_term (term : term) ~(p : psi_def) : (term * term) list * variable list =
  Set.fold
    ~init:([], p.psi_reference.pargs)
    ~f:(fun (rec_elim, vars) var ->
      match Variable.vtype var with
      | None -> rec_elim, var :: vars
      | Some t ->
        if RType.is_recursive t
        then (
          match Expand.mk_recursion_elimination_term p with
          | None -> rec_elim, vars
          | Some (a, _) ->
            (* When are a and b different? *)
            (mk_var var, a) :: rec_elim, vars @ Set.elements (Analysis.free_variables a))
        else rec_elim, var :: vars)
    (Analysis.free_variables term)
;;

let flatten_rec_elim_tuples elim =
  List.concat_map
    ~f:(fun (a, b) ->
      match b.tkind with
      | TTup comps -> List.mapi comps ~f:(fun i t -> mk_sel a i, t)
      | _ -> [ a, b ])
    elim
;;

let subs_from_elim_to_elim elim1 elim2 : (term * term) list =
  List.concat_map
    ~f:(fun (a, b) ->
      let rec f lst =
        match lst with
        | [] ->
          Log.debug_msg
            (Fmt.str
               "Failed to get subs from ctex to term elims when matching %a."
               pp_term
               a);
          []
        | (a', b') :: tl -> if Terms.(equal a a') then [ b', b ] else f tl
      in
      f (flatten_rec_elim_tuples elim2))
    (flatten_rec_elim_tuples elim1)
;;

let term_state_of_context ~(is_pos_ctex : bool) (ctex : ctex) : term_state_detail =
  let scalar_vars = Map.keys ctex.ctex_model in
  let input_args_t = List.map ~f:Variable.vtype_or_new scalar_vars in
  let lemma_f =
    Variable.mk
      ~t:(Some (RType.fun_typ_pack input_args_t TBool))
      (Alpha.fresh ~s:"lemma" ())
  in
  { term = ctex.ctex_eqn.eterm
  ; lemmas = []
  ; lemma = lemma_f
  ; lemma_candidate = None
  ; negative_ctexs = (if is_pos_ctex then [] else [ ctex ])
  ; positive_ctexs = (if is_pos_ctex then [ ctex ] else [])
  ; recurs_elim = ctex.ctex_eqn.eelim
  ; scalar_vars = Map.keys ctex.ctex_model
  ; current_preconds = ctex.ctex_eqn.eprecond
  }
;;

(** Given a counterexample, create a new term state that contains that counterexample,
  or if a term state exists for the term associated with that counterexample,
  update the term state by adding the counterexample's models.
*)
let create_or_update_term_state_with_ctex
    ~(is_pos_ctex : bool)
    (ts : term_state)
    (ctex : ctex)
    : term_state
  =
  match Map.find ts ctex.ctex_eqn.eterm with
  | None ->
    Log.debug (fun fmt () ->
        Fmt.pf fmt "Creating new term state for term@;%a" pp_term ctex.ctex_eqn.eterm);
    Map.add_exn
      ~key:ctex.ctex_eqn.eterm
      ~data:(term_state_of_context ~is_pos_ctex ctex)
      ts
  | Some _ ->
    Map.update ts ctex.ctex_eqn.eterm ~f:(fun maybe_det ->
        match maybe_det with
        | None -> failwith "Term detail does not exist."
        | Some det ->
          if is_pos_ctex
          then { det with positive_ctexs = ctex :: det.positive_ctexs }
          else
            { det with
              current_preconds =
                (match ctex.ctex_eqn.eprecond with
                | None -> None
                | Some pre ->
                  let pre' =
                    substitution
                      (subs_from_elim_to_elim det.recurs_elim ctex.ctex_eqn.eelim)
                      pre
                  in
                  Some pre')
            ; negative_ctexs = ctex :: det.negative_ctexs
            ; positive_ctexs = []
            })
;;

(* ============================================================================================= *)
(*                                  Lemma synthesis    functions                                 *)
(* ============================================================================================= *)

(** Applying tinv to the term of a term detail can help in findiing a good lemma.
  This function extract a skeleton for the lemma when the input is the application of
  tinv to the term of the term detail.
 *)
let skeleton_of_tinv (det : term_state_detail) (tinv_of_t : term) =
  let arg_num var =
    Option.(List.findi ~f:(fun _ a -> Variable.equal a var) det.scalar_vars >>| fst)
  in
  let _tuple_of_args args =
    VarSet.union_list
      (List.map
         ~f:(fun arg ->
           Analysis.free_variables
             ~include_functions:false
             (substitution det.recurs_elim arg))
         args)
  in
  let tinv_of_t' = Reduce.reduce_term ~projecting:true tinv_of_t in
  let case _ t =
    match t.tkind with
    | TSel ({ tkind = TApp (f, _); _ }, i) ->
      let _, tout = RType.fun_typ_unpack (type_of f) in
      (match tout with
      | RType.TTup tl ->
        (match List.nth tl i with
        | Some RType.TBool -> Some (mk_const (Constant.of_bool true))
        | Some RType.TInt -> Some (mk_const (Constant.of_int 0))
        | _ -> None)
      | _ -> None)
    | TApp (f, _) ->
      let _, tout = RType.fun_typ_unpack (type_of f) in
      (match tout with
      | RType.TBool -> Some (mk_const (Constant.of_bool true))
      | RType.TInt -> Some (mk_const (Constant.of_int 0))
      | _ -> None)
    | _ -> None
  in
  let tinv_of_t'' =
    transform_at_depth
      2
      ~case:(fun _ t ->
        match type_of t with
        | RType.TInt -> Some (mk_const (Constant.of_int 0))
        | RType.TBool -> Some (mk_const (Constant.of_bool true))
        | _ -> None)
      (transform ~case tinv_of_t')
  in
  (* ep_term tinv_of_t''); *)
  let expr = Expression.of_term tinv_of_t'' in
  let expr_transform _ e =
    match e with
    | Expression.EVar v ->
      Option.(
        Expression.get_var v
        >>= arg_num
        >>| fun i -> Expression.EBox (Expression.Position i))
    | _ -> None
  in
  Option.bind
    ~f:(fun expr -> Skeleton.of_expression (Expression.transform expr_transform expr))
    expr
;;

(** Construct the synthesis objective corresponding to the lemma of the term state detail.
  For each synthesis strategy implemented, return a command that defines the synthesis
  objective (SyGuS command).
  All strategies should be executed in order to find a solution efficiently.
 *)
let synthfun_of_det ~(p : psi_def) (det : term_state_detail) : (command * string) list =
  let opset =
    List.fold
      ~init:OpSet.empty
      ~f:(fun acc func -> Set.union acc (Analysis.operators_of func.f_body))
      (PMRS.func_of_pmrs p.psi_reference
      @ PMRS.func_of_pmrs p.psi_repr
      @
      match p.psi_tinv with
      | None -> []
      | Some pmrs -> PMRS.func_of_pmrs pmrs)
  in
  let gen_of_guess t guess =
    let grammar =
      Grammars.generate_grammar
        ~short_predicate:t
        ~guess
        ~bools:true
        opset
        det.scalar_vars
        RType.TBool
    in
    let logic = logic_of_operators opset in
    mk_synthinv det.lemma.vname det.scalar_vars grammar, logic
  in
  if !Config.Optims.make_partial_lemma_sketches
  then (
    let skeleton_guess =
      match p.psi_tinv with
      | Some tinv -> skeleton_of_tinv det (Reduce.reduce_pmrs tinv det.term)
      | _ -> None
    in
    [ gen_of_guess true skeleton_guess; gen_of_guess false skeleton_guess ])
  else [ gen_of_guess false None ]
;;

let term_var_string term : string =
  match Set.elements (Analysis.free_variables term) with
  | [] ->
    failwith (Fmt.str "Failed to extract string of variable name in term %a" pp_term term)
  | var :: _ -> var.vname
;;

let convert_term_rec_to_ctex_rec
    ~(p : psi_def)
    (det : term_state_detail)
    (ctex : ctex)
    (name : string)
    : string
  =
  let rec g recvar lst =
    match lst with
    | [] ->
      failwith
        (Fmt.str "Could not find name %s in the ctex's recursion elimination." name)
    | (a, b) :: tl ->
      let i = term_var_string b in
      let l = term_var_string a in
      if String.(equal l recvar) then i else g recvar tl
  in
  let rec h recvar n elim =
    match elim with
    | [] ->
      failwith
        (Fmt.str "Could not find ctex rec elim entry for tuple entry named %s." name)
    | (a, b) :: tl ->
      let l = term_var_string a in
      if String.(equal l recvar)
      then (
        match b.tkind with
        | TTup vars -> term_var_string (List.nth_exn vars n)
        | _ ->
          failwith
            Fmt.(
              str "Cannot get tuple entry %s in ctex rec elim; %s is not a tuple.)" name l))
      else h recvar n tl
  in
  let rec f lst =
    match lst with
    | [] ->
      failwith
        (Fmt.str "Could not find name %s in this term's recursion elimination." name)
    | (a, b) :: tl ->
      (match b.tkind with
      | TTup vars ->
        (match
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
;;

let ctex_model_to_args
    ~(p : psi_def)
    (det : term_state_detail)
    (params : (string * sygus_sort) list)
    ctex
    : sygus_term list
  =
  List.map params ~f:(fun (param_name, _) ->
      match
        let name = convert_term_rec_to_ctex_rec ~p det ctex param_name in
        Map.find
          ctex.ctex_model
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
              (Fmt.str
                 "Failed to extract argument list from ctex model (%s unknown)."
                 name)
          | Some v -> v)
      with
      | None ->
        Log.error Fmt.(fun fmt () -> pf fmt "I was looking for %s" param_name);
        Log.error Fmt.(fun fmt () -> pf fmt "The ctex: %a" pp_ctex ctex);
        failwith "Failed to extract argument list from ctex model."
      | Some t -> sygus_of_term t)
;;

let constraint_of_neg_ctex (det : term_state_detail) ctex =
  let neg_constraint = mk_un Not (mk_app (mk_var det.lemma) (Map.data ctex.ctex_model)) in
  CConstraint (sygus_of_term neg_constraint)
;;

let constraint_of_pos_ctex (det : term_state_detail) ctex =
  let pos_constraint = mk_app (mk_var det.lemma) (Map.data ctex.ctex_model) in
  CConstraint (sygus_of_term pos_constraint)
;;

let log_soln s vs t =
  Log.verbose (fun frmt () ->
      Fmt.pf
        frmt
        "Lemma candidate: \"%s %s = @[%a@]\"."
        s
        (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vs))
        pp_term
        t)
;;

let handle_lemma_synth_response
    (det : term_state_detail)
    ((task, resolver) : solver_response option Lwt.t * int Lwt.u)
    : term list option
  =
  let parse_synth_fun (_, _, _, fbody) =
    let body, _ =
      infer_type (term_of_sygus (VarSet.to_env (VarSet.of_list det.scalar_vars)) fbody)
    in
    body
  in
  match
    Lwt_main.run
      (Lwt.wakeup resolver 0;
       task)
  with
  | Some (RSuccess resps) ->
    let soln = List.map ~f:parse_synth_fun resps in
    let _ = List.iter ~f:(fun t -> log_soln det.lemma.vname det.scalar_vars t) soln in
    Some soln
  | Some RInfeasible | Some RFail | Some RUnknown | None -> None
;;

(* ============================================================================================= *)
(*                                  Lemma verification functions                                 *)
(* ============================================================================================= *)

let mk_f_compose_r_orig ~(p : psi_def) (t : term) : term =
  let repr_of_v = if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pvar [ t ] in
  mk_app_v p.psi_reference.pvar (List.map ~f:mk_var p.psi_reference.pargs @ [ repr_of_v ])
;;

let mk_f_compose_r_main ~(p : psi_def) (t : term) : term =
  let repr_of_v =
    if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pmain_symb [ t ]
  in
  mk_app_v p.psi_reference.pmain_symb [ repr_of_v ]
;;

let get_lemma ~(p : psi_def) (ts : term_state) ~(key : term) : term option =
  let term_detail_to_lemma det =
    let subst =
      List.concat_map
        ~f:(fun (t1, t2) ->
          let frt1 = mk_f_compose_r_main ~p t1 in
          match t2.tkind with
          | TTup t2s -> List.mapi t2s ~f:(fun i t2_i -> t2_i, mk_sel frt1 i)
          | _ -> [ t2, frt1 ])
        det.recurs_elim
    in
    let f lem = Term.substitution subst lem in
    Option.map ~f:simplify_term (T.mk_assoc Binop.And (List.map ~f det.lemmas))
  in
  match Map.find ts key with
  | None -> None
  | Some det -> term_detail_to_lemma det
;;

let smt_of_recurs_elim_eqns (elim : (term * term) list) ~(p : psi_def) : S.smtTerm =
  let lst =
    List.map
      ~f:(fun (t1, t2) ->
        S.mk_eq (smt_of_term (mk_f_compose_r_orig ~p t1)) (smt_of_term t2))
      elim
  in
  if equal (List.length lst) 0 then S.mk_true else S.mk_assoc_and lst
;;

let smt_of_aux_ensures ~(p : psi_def) : S.smtTerm list =
  let mk_sort maybe_rtype =
    match maybe_rtype with
    | None -> S.mk_int_sort
    | Some rtype -> sort_of_rtype rtype
  in
  let pmrss : PMRS.t list =
    [ p.psi_reference; p.psi_target; p.psi_reference ]
    @
    match p.psi_tinv with
    | None -> []
    | Some tinv -> [ tinv ]
  in
  let vars : variable list =
    List.concat
      (List.map
         ~f:(fun (pmrs : PMRS.t) ->
           List.filter
             ~f:(fun v ->
               (not Variable.(v = pmrs.pmain_symb)) && not Variable.(v = pmrs.pvar))
             (Set.elements pmrs.pnon_terminals))
         pmrss)
  in
  List.fold
    ~init:[]
    ~f:(fun acc v ->
      let maybe_ens = Specifications.get_ensures v in
      match maybe_ens with
      | None -> acc
      | Some t ->
        let arg_types = fst (RType.fun_typ_unpack (Variable.vtype_or_new v)) in
        let arg_vs =
          List.map ~f:(fun t -> Variable.mk ~t:(Some t) (Alpha.fresh ())) arg_types
        in
        let args = List.map ~f:mk_var arg_vs in
        let quants =
          List.map
            ~f:(fun var -> S.SSimple var.vname, mk_sort (Variable.vtype var))
            arg_vs
        in
        let ens = Reduce.reduce_term (mk_app t [ mk_app_v v args ]) in
        let smt = S.mk_forall quants (smt_of_term ens) in
        smt :: acc)
    vars
;;

let smt_of_tinv_app ~(p : psi_def) (det : term_state_detail) =
  match p.psi_tinv with
  | None -> failwith "No TInv has been specified. Cannot make smt of tinv app."
  | Some pmrs -> S.mk_simple_app pmrs.pvar.vname [ smt_of_term det.term ]
;;

let smt_of_lemma_app (det : term_state_detail) =
  S.mk_simple_app
    det.lemma.vname
    (List.map ~f:(fun var -> S.mk_var var.vname) det.scalar_vars)
;;

let smt_of_lemma_validity ~(p : psi_def) (det : term_state_detail) =
  let mk_sort maybe_rtype =
    match maybe_rtype with
    | None -> S.mk_int_sort
    | Some rtype -> sort_of_rtype rtype
  in
  let quants =
    List.map
      ~f:(fun var -> S.SSimple var.vname, mk_sort (Variable.vtype var))
      (Set.elements (Analysis.free_variables det.term)
      @ p.psi_reference.pargs
      @ List.concat_map
          ~f:(fun (_, b) -> Set.elements (Analysis.free_variables b))
          det.recurs_elim)
  in
  let preconds =
    match det.current_preconds with
    | None -> []
    | Some pre -> [ smt_of_term pre ]
  in
  let if_condition =
    S.mk_assoc_and
      ([ smt_of_tinv_app ~p det; smt_of_recurs_elim_eqns det.recurs_elim ~p ] @ preconds)
  in
  let if_then = smt_of_lemma_app det in
  [ S.mk_assert (S.mk_not (S.mk_forall quants (S.mk_or (S.mk_not if_condition) if_then)))
  ]
;;

let set_up_lemma_solver solver ~(p : psi_def) (det : term_state_detail) =
  let preamble =
    Commands.mk_preamble
      ~logic:
        (SmtLogic.infer_logic
           ~quantifier_free:false
           ~with_uninterpreted_functions:true
           ~for_induction:true
           ~logic_infos:(AState.psi_def_logics p)
           [])
      ~incremental:false
      ~induction:true
      ~models:true
      ()
  in
  let lemma_body = det.lemma_candidate in
  match lemma_body with
  | Some lemma_body ->
    let%lwt () = AsyncSmt.exec_all solver preamble in
    let%lwt () =
      Lwt_list.iter_p
        (fun x ->
          let%lwt _ = AsyncSmt.exec_command solver x in
          return ())
        ((* Start by defining tinv. *)
         Option.(map ~f:smt_of_pmrs p.psi_tinv |> value ~default:[])
        (* PMRS definitions.*)
        (* Reference function. *)
        @ smt_of_pmrs p.psi_reference
        (* Representation function. *)
        @ (if p.psi_repr_is_identity then [] else smt_of_pmrs p.psi_repr)
        (* Assert invariants on functions *)
        @ List.map ~f:S.mk_assert (smt_of_aux_ensures ~p)
        (* Declare lemmas. *)
        @ [ Commands.mk_def_fun
              det.lemma.vname
              (List.map ~f:(fun v -> v.vname, Variable.vtype_or_new v) det.scalar_vars)
              RType.TBool
              lemma_body
          ])
    in
    return ()
  | None ->
    (* Nothing to do! *)
    return ()
;;

let smt_of_disallow_ctex_values (det : term_state_detail) : S.smtTerm =
  let ctexs = det.positive_ctexs in
  let of_one_ctex ctex =
    Map.fold
      ~f:(fun ~key ~data acc ->
        let var =
          let subs = subs_from_elim_to_elim det.recurs_elim ctex.ctex_eqn.eelim in
          Term.substitution subs (mk_var key)
        in
        smt_of_term Terms.(var == data) :: acc)
      ~init:[]
      ctex.ctex_model
  in
  S.mk_assoc_and
    (List.map ~f:(fun ctex -> S.mk_not (S.mk_assoc_and (of_one_ctex ctex))) ctexs)
;;

let set_up_to_get_model solver ~(p : psi_def) (det : term_state_detail) =
  (* Step 1. Declare vars for term, and assert that term satisfies tinv. *)
  let%lwt () =
    AsyncSmt.exec_all solver (Commands.decls_of_vars (Analysis.free_variables det.term))
  in
  let%lwt _ = AsyncSmt.smt_assert solver (smt_of_tinv_app ~p det) in
  (* Step 2. Declare scalars (vars for recursion elimination & spec param) and their constraints (preconds & recurs elim eqns) *)
  let%lwt () =
    AsyncSmt.exec_all solver (Commands.decls_of_vars (VarSet.of_list det.scalar_vars))
  in
  let%lwt _ =
    AsyncSmt.exec_command
      solver
      (S.mk_assert (smt_of_recurs_elim_eqns det.recurs_elim ~p))
  in
  let%lwt () =
    match det.current_preconds with
    | None -> return ()
    | Some pre -> AsyncSmt.smt_assert solver (smt_of_term pre)
  in
  (* Step 3. Disallow repeated positive examples. *)
  let%lwt () =
    if List.length det.positive_ctexs > 0
    then AsyncSmt.smt_assert solver (smt_of_disallow_ctex_values det)
    else return ()
  in
  (* Step 4. Assert that lemma candidate is false. *)
  AsyncSmt.smt_assert solver (S.mk_not (smt_of_lemma_app det))
;;

let mk_model_sat_asserts det f_o_r instantiate =
  let f v =
    let v_val = mk_var (List.find_exn ~f:(fun x -> equal v.vid x.vid) det.scalar_vars) in
    match List.find_map ~f:(find_original_var_and_proj v) det.recurs_elim with
    | Some (original_recursion_var, proj) ->
      (match original_recursion_var.tkind with
      | TVar ov when Option.is_some (instantiate ov) ->
        let instantiation = Option.value_exn (instantiate ov) in
        if proj >= 0
        then (
          let t = Reduce.reduce_term (mk_sel (f_o_r instantiation) proj) in
          smt_of_term Terms.(t == v_val))
        else smt_of_term Terms.(f_o_r instantiation == v_val)
      | _ ->
        Log.error_msg
          Fmt.(str "Warning: skipped instantiating %a." pp_term original_recursion_var);
        SmtLib.mk_true)
    | None -> smt_of_term Terms.(mk_var v == v_val)
  in
  List.map ~f det.scalar_vars
;;

let verify_lemma_bounded ~(p : psi_def) (det : term_state_detail)
    : (Utils.Stats.verif_method * S.solver_response) Lwt.t * int Lwt.u
  =
  let logic = SmtLogic.infer_logic ~logic_infos:(AState.psi_def_logics p) [] in
  let task (solver, starter) =
    let%lwt _ = starter in
    let%lwt () =
      AsyncSmt.exec_all
        solver
        Commands.(
          mk_preamble
            ~incremental:(String.is_prefix ~prefix:"CVC" solver.s_name)
            ~logic
            ()
          @ decls_of_vars (VarSet.of_list det.scalar_vars))
    in
    let steps = ref 0 in
    let rec check_bounded_sol accum terms =
      let f accum t =
        let rec_instantation =
          Option.value ~default:VarMap.empty (Matching.matches t ~pattern:det.term)
        in
        let%lwt _ = accum in
        let f_compose_r t =
          let repr_of_v =
            if p.psi_repr_is_identity then t else Reduce.reduce_pmrs p.psi_repr t
          in
          Reduce.reduce_term (Reduce.reduce_pmrs p.psi_reference repr_of_v)
        in
        let subs =
          List.map
            ~f:(fun (orig_rec_var, elimv) ->
              match orig_rec_var.tkind with
              | TVar rec_var when Map.mem rec_instantation rec_var ->
                elimv, f_compose_r (Map.find_exn rec_instantation rec_var)
              | _ -> failwith "all elimination variables should be substituted.")
            det.recurs_elim
          (* Map.fold ~init:[] ~f:(fun ~key ~data acc -> (mk_var key, data) :: acc) rec_instantation *)
        in
        let preconds =
          Option.to_list
            (Option.map ~f:(fun t -> substitution subs t) det.current_preconds)
        in
        let model_sat =
          mk_model_sat_asserts det f_compose_r (Map.find rec_instantation)
        in
        let%lwt () = AsyncSmt.spush solver in
        (* Assert that preconditions hold and map original term's recurs elim variables to the variables that they reduce to for this concrete term. *)
        let%lwt _ =
          AsyncSmt.exec_all
            solver
            (Commands.decls_of_vars
               (List.fold
                  ~init:VarSet.empty
                  ~f:(fun acc t -> Set.union acc (Analysis.free_variables t))
                  (preconds @ Map.data rec_instantation)))
        in
        let%lwt () =
          AsyncSmt.smt_assert
            solver
            (SmtLib.mk_assoc_and (List.map ~f:smt_of_term preconds @ model_sat))
        in
        (* Assert that TInv is true for this concrete term t *)
        let%lwt _ =
          match p.psi_tinv with
          | None -> return ()
          | Some tinv ->
            let tinv_t = Reduce.reduce_pmrs tinv t in
            let%lwt _ =
              AsyncSmt.exec_all
                solver
                (Commands.decls_of_vars (Analysis.free_variables tinv_t))
            in
            let%lwt _ = AsyncSmt.smt_assert solver (smt_of_term tinv_t) in
            return ()
        in
        (* Assert that lemma is false for this concrete term t  *)
        let%lwt _ =
          match det.lemma_candidate with
          | Some lemma ->
            AsyncSmt.exec_command
              solver
              (S.mk_assert (S.mk_not (smt_of_term (substitution subs lemma))))
          | _ -> return S.Unknown
        in
        let%lwt resp = AsyncSmt.check_sat solver in
        (* Note that I am getting a model after check-sat unknown response. This may not halt.  *)
        let%lwt result =
          match resp with
          | SmtLib.Sat | SmtLib.Unknown ->
            let%lwt model = AsyncSmt.get_model solver in
            return (Some model)
          | _ -> return None
        in
        let%lwt () = AsyncSmt.spop solver in
        return (resp, result)
      in
      match terms with
      | [] -> accum
      | t0 :: tl ->
        let%lwt accum' = f accum t0 in
        (match accum' with
        | status, Some model -> return (status, Some model)
        | _ -> check_bounded_sol (return accum') tl)
    in
    let rec expand_loop u =
      match Set.min_elt u, !steps < !Config.Optims.num_expansions_check with
      | Some t0, true ->
        let tset, u' = Expand.simple t0 in
        let%lwt check_result =
          check_bounded_sol (return (SmtLib.Unknown, None)) (Set.elements tset)
        in
        steps := !steps + Set.length tset;
        (match check_result with
        | _, Some model ->
          Log.debug_msg
            "Bounded lemma verification has found a counterexample to the lemma \
             candidate.";
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
    let* res = expand_loop (TermSet.singleton det.term) in
    let* () = AsyncSmt.close_solver solver in
    return (Utils.Stats.BoundedChecking, res)
  in
  AsyncSmt.(cancellable_task (make_solver "cvc") task)
;;

let verify_lemma_unbounded ~(p : psi_def) (det : term_state_detail)
    : (Utils.Stats.verif_method * S.solver_response) Lwt.t * int Lwt.u
  =
  let build_task (cvc4_instance, task_start) =
    let%lwt _ = task_start in
    let%lwt () = set_up_lemma_solver cvc4_instance ~p det in
    let%lwt () =
      (Lwt_list.iter_p (fun x ->
           let%lwt _ = AsyncSmt.exec_command cvc4_instance x in
           return ()))
        (smt_of_lemma_validity ~p det)
    in
    let%lwt resp = AsyncSmt.check_sat cvc4_instance in
    let%lwt final_response =
      match resp with
      | Sat | Unknown ->
        let%lwt _ = set_up_to_get_model cvc4_instance ~p det in
        let%lwt resp' = AsyncSmt.check_sat cvc4_instance in
        (match resp' with
        | Sat | Unknown -> AsyncSmt.get_model cvc4_instance
        | _ -> return resp')
      | _ -> return resp
    in
    let%lwt () = AsyncSmt.close_solver cvc4_instance in
    Log.debug_msg "Unbounded lemma verification is complete.";
    return (Utils.Stats.Induction, final_response)
  in
  AsyncSmt.(cancellable_task (AsyncSmt.make_solver "cvc") build_task)
;;

(** Verify a lemma candidate with two parallel calls: one call to an induction solver,
  another call to a SMT solver. The induction solver attempts to prove that the solution
  is correct while the smt solver attempt to find a counterexample.
*)
let verify_lemma_candidate ~(p : psi_def) (det : term_state_detail)
    : Utils.Stats.verif_method * SyncSmt.solver_response
  =
  match det.lemma_candidate with
  | None -> failwith "Cannot verify lemma candidate; there is none."
  | Some _ ->
    Log.verbose (fun f () -> Fmt.(pf f "Checking lemma candidate..."));
    let resp =
      try
        Lwt_main.run
          (let pr1, resolver1 = verify_lemma_bounded ~p det in
           let pr2, resolver2 = verify_lemma_unbounded ~p det in
           Lwt.wakeup resolver2 1;
           Lwt.wakeup resolver1 1;
           (* The first call to return is kept, the other one is ignored. *)
           Lwt.pick [ pr1; pr2 ])
      with
      | End_of_file ->
        Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
        Log.error_msg "Please inspect logs.";
        BoundedChecking, SmtLib.Unknown
    in
    resp
;;

let parse_positive_example_solver_model response (det : term_state_detail) =
  match response with
  | SmtLib.SExps s ->
    let model = model_to_constmap (SExps s) in
    let m, _ =
      Map.partitioni_tf
        ~f:(fun ~key ~data:_ ->
          Option.is_some (VarSet.find_by_name (VarSet.of_list det.scalar_vars) key))
        model
    in
    (* Remap the names to ids of the original variables in m' *)
    [ ({ (placeholder_ctex det) with
         ctex_model =
           Map.fold
             ~init:VarMap.empty
             ~f:(fun ~key ~data acc ->
               match VarSet.find_by_name (VarSet.of_list det.scalar_vars) key with
               | None ->
                 Log.info (fun f () -> Fmt.(pf f "Could not find by name %s" key));
                 acc
               | Some var -> Map.set ~data acc ~key:var)
             m
       }
        : ctex)
    ]
  | _ ->
    failwith
      "Parse model failure: Positive example cannot be found during lemma refinement."
;;

(** The Interactive module contains function used in interactive mode. *)
module Interactive = struct
  let make_term_state_detail ~(p : psi_def) (term : term) : term_state_detail =
    let recurs_elim, scalar_vars = recurs_elim_of_term ~p term in
    let input_args_t = List.map ~f:Variable.vtype_or_new scalar_vars in
    let lemma_f =
      Variable.mk
        ~t:(Some (RType.fun_typ_pack input_args_t TBool))
        (Alpha.fresh ~s:"lemma" ())
    in
    { term
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

  let classify_ctexs_opt ctexs : ctex list =
    let f ctex =
      Log.info (fun frmt () ->
          Fmt.(pf frmt "Classify this counterexample: %a (P/N/U)" (box pp_ctex) ctex));
      match Stdio.In_channel.input_line Stdio.stdin with
      | Some "N" ->
        { ctex with ctex_stat = add_cause ctex.ctex_stat ViolatesTargetRequires }
      | Some "P" -> { ctex with ctex_stat = Valid }
      | _ -> ctex
    in
    List.map ~f ctexs
  ;;

  let set_term_lemma ~(p : psi_def) (ts : term_state) ~(key : term) ~(lemma : term)
      : term_state
    =
    match Map.find ts key with
    | None ->
      Map.add_exn
        ts
        ~key
        ~data:{ (make_term_state_detail ~p key) with lemmas = [ lemma ] }
    | Some det -> Map.add_exn ts ~key ~data:{ det with lemmas = [ lemma ] }
  ;;

  let add_lemmas ~(p : psi_def) (lstate : refinement_loop_state) : refinement_loop_state =
    let env_in_p = VarSet.of_list p.psi_reference.pargs in
    let f existing_lemmas t =
      let vars = Set.union (Analysis.free_variables t) env_in_p in
      let env = VarSet.to_env vars in
      Log.info (fun frmt () ->
          Fmt.pf frmt "Please provide a constraint for \"@[%a@]\"." pp_term t);
      Log.verbose (fun frmt () ->
          Fmt.pf
            frmt
            "Environment:@;@[functions %s, %s and %s@]@;and @[%a@]."
            p.psi_reference.pvar.vname
            p.psi_target.pvar.vname
            p.psi_repr.pvar.vname
            VarSet.pp
            vars);
      match Stdio.In_channel.input_line Stdio.stdin with
      | None | Some "" ->
        Log.info (fun frmt () -> Fmt.pf frmt "No additional constraint provided.");
        existing_lemmas
      | Some x ->
        let smtterm =
          try
            let sexpr = Sexplib.Sexp.of_string x in
            Smtlib.SmtLib.smtTerm_of_sexp sexpr
          with
          | Failure _ -> None
        in
        let pred_term = term_of_smt env in
        let term x =
          match get_lemma ~p existing_lemmas ~key:t with
          | None -> pred_term x
          | Some inv -> mk_bin Binop.And inv (pred_term x)
        in
        (match smtterm with
        | None -> existing_lemmas
        | Some x -> set_term_lemma ~p existing_lemmas ~key:t ~lemma:(term x))
    in
    { lstate with term_state = Set.fold ~f ~init:lstate.term_state lstate.t_set }
  ;;

  let parse_interactive_positive_example (det : term_state_detail) (input : string)
      : ctex option
    =
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

  let interactive_get_positive_examples (det : term_state_detail) =
    let vars =
      Set.filter
        ~f:(fun var ->
          match Variable.vtype var with
          | None -> false
          | Some t -> not (RType.is_recursive t))
        (Analysis.free_variables det.term)
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
                    ^ (match Variable.vtype var with
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

  let interactive_check_lemma lemma_refinement_loop name vars lemma_term det =
    Log.info (fun f () ->
        Fmt.(
          pf
            f
            "Is the lemma \"%s %s = @[%a@]\" for term %a[%a] correct? [Y/N]"
            name
            (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) vars))
            pp_term
            lemma_term
            pp_term
            det.term
            pp_subs
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
            positive_ctexs = det.positive_ctexs @ interactive_get_positive_examples det
          }
      | _ -> None)
  ;;
end

let synthesize_new_lemma ~(p : psi_def) (det : term_state_detail) : term option =
  let with_synth_obj i synth_obj logic =
    AlgoLog.announce_new_lemma_synthesis i det;
    let neg_constraints = List.map ~f:(constraint_of_neg_ctex det) det.negative_ctexs in
    let pos_constraints = List.map ~f:(constraint_of_pos_ctex det) det.positive_ctexs in
    let extra_defs = [ max_definition; min_definition ] in
    let commands =
      CSetLogic logic
      :: (extra_defs @ [ synth_obj ] @ neg_constraints @ pos_constraints @ [ CCheckSynth ])
    in
    match
      handle_lemma_synth_response det (SygusInterface.SygusSolver.solve_commands commands)
    with
    | None -> None
    | Some solns -> List.nth solns 0
  in
  match synthfun_of_det ~p det with
  | [ (synth_obj, logic) ] -> with_synth_obj 0 synth_obj logic
  | obj_choices ->
    let lwt_tasks =
      List.mapi obj_choices ~f:(fun i (synth_obj, logic) ->
          Lwt.task ()
          |> fun (t, r) -> Lwt.map (fun _ -> with_synth_obj i synth_obj logic) t, r)
    in
    Lwt_main.run
      (Lwt.pick
         (List.map
            ~f:(fun (t, r) ->
              Lwt.wakeup r 0;
              t)
            lwt_tasks))
;;

(* ============================================================================================= *)
(*                                  Main entry points                                            *)
(* ============================================================================================= *)

let rec lemma_refinement_loop ~(p : psi_def) (det : term_state_detail)
    : term_state_detail option
  =
  match synthesize_new_lemma ~p det with
  | None ->
    Log.debug_msg "Lemma synthesis failure.";
    None
  | Some lemma_term ->
    if !Config.interactive_check_lemma
    then
      Interactive.interactive_check_lemma
        (lemma_refinement_loop ~p)
        det.lemma.vname
        det.scalar_vars
        lemma_term
        det
    else (
      match verify_lemma_candidate ~p { det with lemma_candidate = Some lemma_term } with
      | vmethod, Unsat ->
        AlgoLog.lemma_proved_correct vmethod det lemma_term;
        let lemma =
          match det.current_preconds with
          | None -> lemma_term
          | Some _pre -> lemma_term
          (* mk_bin Binop.And (mk_un Unop.Not pre) lemma_term *)
        in
        Some { det with lemma_candidate = None; lemmas = lemma :: det.lemmas }
      | vmethod, SmtLib.SExps x ->
        AlgoLog.lemma_not_proved_correct vmethod;
        let new_positive_ctexs =
          parse_positive_example_solver_model (SmtLib.SExps x) det
        in
        List.iter
          ~f:(fun ctex ->
            Log.verbose (fun f () ->
                Fmt.(pf f "Found a positive example: %a" (box pp_ctex) ctex)))
          new_positive_ctexs;
        lemma_refinement_loop
          ~p
          { det with positive_ctexs = det.positive_ctexs @ new_positive_ctexs }
      | _, Sat ->
        Log.error_msg "Lemma verification returned Sat. This is unexpected.";
        None
      | _, Unknown ->
        Log.error_msg "Lemma verification returned Unknown.";
        None
      | _ ->
        Log.error_msg "Lemma verification is indeterminate.";
        None)
;;

(** Partitioning function to partitiion a list into (a,b,c) where a are
  examples that satisfy the invariant,
  b are examples that do not satisfy the invariant,
  c are examples that are spurious for other reasons.
*)
let ctexs_for_lemma_synt ctex =
  match ctex.ctex_stat with
  | Valid -> `Fst ctex
  | Spurious causes ->
    if Caml.List.mem ViolatesTargetRequires causes then `Snd ctex else `Trd ctex
  | _ -> `Trd ctex
;;

(** Partitioning function to partitiion a list into (a,b,c) where a are
  examples that are not in the reference function's image,
  b are examples that are in the reference function's image,
  c are examples that are spurious for other reasons.
*)
let ctexs_for_ensures_synt ctex =
  match ctex.ctex_stat with
  | Valid -> `Fst ctex
  | Spurious causes ->
    if Caml.List.mem NotInReferenceImage causes then `Snd ctex else `Trd ctex
  | _ -> `Trd ctex
;;

let refine_ensures_predicates
    ~(p : psi_def)
    ~(neg_ctexs : ctex list)
    ~(pos_ctexs : ctex list)
    (lstate : refinement_loop_state)
    : term_state * [ `CoarseningOk | `CoarseningFailure | `Unrealizable ]
  =
  Log.info
    Fmt.(
      fun fmt () ->
        pf fmt "%i counterexamples violate image assumption." (List.length neg_ctexs));
  let maybe_pred = ImagePredicates.synthesize ~p pos_ctexs neg_ctexs [] in
  match maybe_pred with
  | None -> lstate.term_state, `CoarseningFailure
  | Some ensures ->
    (match Specifications.get_ensures p.psi_reference.pvar with
    | None -> Specifications.set_ensures p.psi_reference.pvar ensures
    | Some old_ensures ->
      let var : variable =
        Variable.mk ~t:(Some p.psi_reference.poutput_typ) (Alpha.fresh ())
      in
      let new_pred =
        mk_fun
          [ FPatVar var ]
          (mk_bin
             Binop.And
             (mk_app old_ensures [ mk_var var ])
             (mk_app ensures [ mk_var var ]))
      in
      Specifications.set_ensures p.psi_reference.pvar new_pred);
    lstate.term_state, `CoarseningOk
;;

let synthesize_lemmas ~(p : psi_def) synt_failure_info (lstate : refinement_loop_state)
    : (refinement_loop_state, solver_response) Result.t
  =
  let _interactive_synthesis () =
    !Config.interactive_lemmas_loop
    &&
    (Log.info (fun frmt () -> Fmt.pf frmt "No luck. Try again? (Y/N)");
     match Stdio.In_channel.input_line Stdio.stdin with
     | None | Some "" | Some "N" -> false
     | Some "Y" -> true
     | _ -> false)
  in
  (*
    Example: the synt_failure_info should be a list of unrealizability counterexamples, which
    are pairs of counterexamples.
    Each counterexample can be classified as positive or negative w.r.t to the predicate p.psi_tinv.
    The lemma corresponding to a particular term should be refined to eliminate the counterexample
    (a counterexample cex is also associated to a particular term through cex.ctex_eqn.eterm)
   *)
  let new_state, lemma_synthesis_success =
    match synt_failure_info with
    | _, Either.First _ -> failwith "There is no synt_failure_info in synthesize_lemmas."
    | _, Either.Second unrealizability_ctexs ->
      (* Forget about the specific association in pairs. *)
      let ctexs = List.concat_map unrealizability_ctexs ~f:(fun uc -> [ uc.ci; uc.cj ]) in
      let classified_ctexs =
        if !Config.classify_ctex
        then Interactive.classify_ctexs_opt ctexs
        else classify_ctexs ~p ctexs
      in
      (* Positive and negatives for the ensures predicates. *)
      let ensures_positives, ensures_negatives, _ =
        List.partition3_map ~f:ctexs_for_ensures_synt classified_ctexs
      in
      (* Positive and negatives for the requires of the target function. *)
      let lemma_synt_positives, lemma_synt_negatives, _ =
        List.partition3_map ~f:ctexs_for_lemma_synt classified_ctexs
      in
      if List.length ensures_negatives > 0
      then
        refine_ensures_predicates
          ~p
          ~neg_ctexs:ensures_negatives
          ~pos_ctexs:ensures_positives
          lstate
      else (
        (* Update the term state by adding the positive and negative counterexamples to it. *)
        let ts : term_state =
          let update is_positive ctexs ts =
            List.fold
              ctexs
              ~init:ts
              ~f:(create_or_update_term_state_with_ctex ~is_pos_ctex:is_positive)
          in
          lstate.term_state
          |> update false lemma_synt_negatives
          |> update true lemma_synt_positives
        in
        if List.is_empty lemma_synt_negatives
        then (
          (* lemma_synt_negatives and ensures_negatives are empty; all ctexs spurious! *)
          AlgoLog.no_spurious_ctex ();
          ts, `Unrealizable)
        else (
          AlgoLog.spurious_violates_requires (List.length lemma_synt_negatives);
          let new_ts, success =
            Map.fold
              ts
              ~init:(Map.empty (module Terms), true)
              ~f:(fun ~key ~data:det (acc, status) ->
                if Analysis.is_bounded det.term
                then acc, status (* Skip lemma synth for bounded terms. *)
                else if not status
                then acc, status
                else (
                  match lemma_refinement_loop det ~p with
                  | None -> acc, false
                  | Some det -> Map.add_exn ~key ~data:det acc, status))
          in
          new_ts, if success then `CoarseningOk else `CoarseningFailure))
  in
  match lemma_synthesis_success with
  | `CoarseningOk -> Ok { lstate with term_state = new_state }
  | `Unrealizable -> Error RInfeasible
  | `CoarseningFailure ->
    (match synt_failure_info with
    | RFail, _ ->
      Log.error_msg "SyGuS solver failed to find a solution.";
      Error RFail
    | RInfeasible, _ ->
      (* Rare - but the synthesis solver can answer "infeasible", in which case it can give
             counterexamples. *)
      AlgoLog.print_infeasible_message lstate.t_set;
      Error RInfeasible
    | RUnknown, _ ->
      (* In most cases if the synthesis solver does not find a solution and terminates, it will
             answer unknowns. We interpret it as "no solution can be found". *)
      Log.error_msg "SyGuS solver returned unknown.";
      Error RUnknown
    | s_resp, _ -> Error s_resp)
;;
