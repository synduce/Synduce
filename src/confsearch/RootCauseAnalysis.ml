open Base
open Common
open Env
open Lang
open ProblemDefs
open Term
open Utils
module Sub = Configuration.Subconf
module G = ConfGraph
module CO = Config.Optims

let find_arg_with_subterm ~(fvar : variable) ~(sub : term) (t : term) =
  let case _ t =
    match t.tkind with
    | TApp ({ tkind = TVar v; _ }, args) when Variable.equal v fvar ->
      (match List.find args ~f:(fun t -> Matching.is_simple_subterm t ~sub) with
      | Some t0 -> Some (TermSet.singleton t0)
      | None -> None)
    | _ -> None
  in
  Term.reduce ~case ~init:TermSet.empty ~join:Set.union t
;;

(** Looks for a set of applicable rules in prules to rewrite (f fargs) and return
    the result of applying the possible rules.
    If there is no rule that is applicable, then return an empty list.
*)
let rule_lookup
    ~(fvar : variable)
    ~(sub : term)
    (ctx : env)
    prules
    (f : variable)
    (fargs : term list)
    (memory : VarSet.t ref)
    : term list
  =
  let match_with_pat (_, rule_args, _, rhs) pat first_args to_pat_match =
    match Matching.matches_pattern to_pat_match pat with
    | Some bindto_map ->
      let bindto_list = Map.to_alist bindto_map in
      let pat_v, pat_bto = List.unzip bindto_list in
      (match List.zip (rule_args @ pat_v) (first_args @ pat_bto) with
      | Ok subst ->
        let rhs' =
          substitution (List.map ~f:(fun (v, t) -> mk_var ctx.ctx v, t) subst) rhs
        in
        (* Find where to obtain sub from *)
        let possible_accessing_calls = find_arg_with_subterm ~fvar ~sub rhs' in
        (* Fmt.(
          pf
            stdout
            "Find %s(..%a..) in %a -> %a@."
            fvar.vname
            (pp_term ctx.ctx)
            sub
            (pp_term ctx.ctx)
            rhs'
            (list ~sep:comma (pp_term ctx.ctx))
            (Set.to_list possible_accessing_calls)); *)
        let can_access t =
          Set.exists possible_accessing_calls ~f:(fun elt ->
              Matching.is_simple_subterm ~sub:t elt)
        in
        (* Add the variable to obtain sub from in memory. *)
        List.iter subst ~f:(fun (v, t) ->
            if can_access t then memory := Set.add !memory v);
        Some rhs'
      | _ -> None)
    | None -> None
  in
  let f (nt, rule_args, rule_pat, rhs) : term option =
    if Variable.(nt = f)
    then (
      match rule_pat with
      (* We have a pattern, try to match it. *)
      | Some pat ->
        (* Separate last argument and the rest. Last argument is pattern-matched. *)
        (match List.last fargs, List.drop_last fargs with
        | Some to_pat_match, Some first_args ->
          let f = match_with_pat (nt, rule_args, rule_pat, rhs) pat first_args in
          let cond_pat = Reduce.CondTree.of_term to_pat_match in
          Reduce.(Option.map ~f:CondTree.to_term CondTree.(all_or_none (map ~f cond_pat)))
        | _ -> None)
      (* Pattern is empty. Simple substitution. *)
      | None ->
        (match List.zip rule_args fargs with
        | Ok subst ->
          Some (substitution (List.map ~f:(fun (v, t) -> mk_var ctx.ctx v, t) subst) rhs)
        | _ -> None))
    else None
  in
  second (List.unzip (Map.to_alist (Map.filter_map prules ~f)))
;;

(**
  reduce_term reduces a term using only the lambda-calculus
*)
let rec reduce_term_with_lookup
    ?(projecting = false)
    ?(unboxing = false)
    ~(memory : VarSet.t ref)
    ~(ctx : env)
    ~(fvar : variable)
    ~(sub : term)
    (t : term)
    : term
  =
  let one_step t =
    let rstep = ref false in
    let case f t =
      let x =
        match t.tkind with
        | TApp (func, args) ->
          let func' = f func in
          let args' = List.map ~f args in
          (match Reduce.resolve_func ctx.functions ctx.ctx func' with
          | FRFun (fpatterns, body) ->
            (match ctx >- Analysis.subst_args fpatterns args' with
            | Ok (remaining_patterns, subst) ->
              (* If there are remaining patterns the application is partial. *)
              (match remaining_patterns with
              | [] -> Some (substitution subst body)
              | rem -> Some (mk_fun ctx.ctx rem (substitution subst body)))
            | Error _ -> None)
          | FRPmrs pm ->
            (match args' with
            | [ tp ] -> Some (f (reduce_pmrs_with_lookup ~memory ~ctx ~fvar ~sub pm tp))
            | _ -> None (* PMRS are defined only with one argument for now. *))
          | FRNonT p -> Some (pmrs_ ~memory ~ctx ~fvar ~sub p (mk_app func' args'))
          | FRUnknown -> Some (mk_app func' args'))
        | TFun ([], body) -> Some (f body)
        | TIte (c, tt, tf) ->
          (match c.tkind with
          (* Resolve constants *)
          | TConst Constant.CFalse -> Some (f tf)
          | TConst Constant.CTrue -> Some (f tt)
          (* Distribute ite on tuples *)
          | _ ->
            (match tt.tkind, tf.tkind with
            | TTup tlt, TTup tlf ->
              (match List.zip tlt tlf with
              | Ok zip ->
                Some
                  (mk_tup ctx.ctx (List.map zip ~f:(fun (tt', tf') -> mk_ite c tt' tf')))
              | Unequal_lengths -> None)
            | _, _ -> None))
        | TSel (t, i) ->
          (match f t with
          | { tkind = TTup tl; _ } -> Some (List.nth_exn tl i)
          | _ -> None)
        | TMatch (t, cases) ->
          (match
             List.filter_opt
               (List.map
                  ~f:(fun (p, t') ->
                    Option.map ~f:(fun m -> m, t') (Matching.matches_pattern t p))
                  cases)
           with
          | [] -> None
          | (subst_map, rhs_t) :: _ ->
            Some (substitution (VarMap.to_subst ctx.ctx subst_map) rhs_t))
        | TBox t -> if unboxing then Some t else None
        | TFun _ | TVar _ | TTup _ | TBin _ | TUn _ | TConst _ | TData _ -> None
      in
      match x with
      | Some x ->
        rstep := true;
        Some x
      | None -> None
    in
    ( transform
        ~case
        (if projecting then Reduce.project_irreducible_terms ctx.ctx t else t)
    , !rstep )
  in
  Reduce.until_irreducible one_step t

and pmrs_
    ~(ctx : env)
    ~(fvar : variable)
    ~(sub : term)
    ~(memory : VarSet.t ref)
    (prog : PMRS.t)
    (input : term)
  =
  let one_step t0 =
    let rstep = ref false in
    let rewrite_rule tm =
      match tm.tkind with
      | TApp ({ tkind = TVar f; _ }, fargs) ->
        (match rule_lookup ~fvar ~sub ctx prog.prules f fargs memory with
        | [] -> tm (* No rule matches *)
        | hd :: _ ->
          (* Only select first match. *)
          rstep := true;
          hd)
      | _ -> tm
    in
    let t0' = rewrite_with rewrite_rule t0 in
    t0', !rstep
  in
  Reduce.until_irreducible one_step input

and reduce_pmrs_with_lookup
    ~(ctx : env)
    ~(fvar : variable)
    ~(sub : term)
    ~(memory : VarSet.t ref)
    (prog : PMRS.t)
    (input : term)
  =
  let f_input = mk_app (mk_var ctx.ctx prog.pmain_symb) [ input ] in
  reduce_term_with_lookup
    ~memory
    ~ctx
    ~fvar
    ~sub
    (pmrs_ ~ctx ~fvar ~sub ~memory prog f_input)
;;

let find_req_arg
    ~(ctx : env)
    ~(g : PMRS.t)
    ~(sub : term)
    ~(xi : variable)
    (s : G.state)
    (_ : Sub.t)
    (input : term)
  =
  (* Fmt.(
    pf
      stdout
      "@[REQ: on input @[%a@], %s should access@;@[%a@]@]@."
      (Term.pp_term ctx.ctx)
      input
      xi.vname
      (Term.pp_term ctx.ctx)
      sub); *)
  let gmax, ctx = Configuration.apply_configuration ~ctx s.st_super g in
  let memory = ref VarSet.empty in
  (* Populate memory with call to reduction with lookup *)
  let _ = reduce_pmrs_with_lookup ~memory ~ctx ~fvar:xi ~sub gmax input in
  (* Fmt.(pf stdout "memory: %a@." (list (Variable.pp ctx.ctx)) (Set.to_list !memory)); *)
  match Map.find s.st_super xi with
  | Some arglist ->
    List.filter_mapi arglist ~f:(fun i arg ->
        if not (Set.are_disjoint (ctx >- Analysis.free_variables arg) !memory)
        then (* Fmt.(pf stdout "Add %i,%i.@." xi.vid i); *)
          Some (xi.vid, i)
        else None)
  | None -> failwith "Not expected."
;;

(* ============================================================================================= *)
(*                            OTHER SUBROUTINES                                                  *)
(* ============================================================================================= *)

let analyze_witness_list
    ~(ctx : env)
    (s : G.state)
    (c : Sub.t)
    (wl : unrealizability_witness list)
  =
  (* TODO not implemented *)
  let _ = ctx, s, c, wl in
  ()
;;

(* failwith "DEBUG" *)

let analyze_reqs
    ~(ctx : env)
    ~(g : PMRS.t)
    (s : G.state)
    (c : Sub.t)
    (_ : unrealizability_witness list)
    (reqs : (term * variable * term) list)
    : (int * int) list
  =
  List.dedup_and_sort
    ~compare:Poly.compare
    (List.concat_map reqs ~f:(fun (t_input, xi_v, t_req) ->
         find_req_arg ~ctx ~g ~xi:xi_v ~sub:t_req s c t_input))
;;

let analyze_witnesses
    ~(ctx : env)
    ~(g : PMRS.t)
    (s : G.state)
    (c : Sub.t)
    (r : repair)
    (wl : unrealizability_witness list)
  =
  match r with
  | Lift ->
    (* Lift repair should be handled by the single-configuration solver. *)
    ()
  | AddRecursiveCalls reqs ->
    (* Local root causing has identified missing information.
      This missing information must be related to precise recursive calls or
      scalar arguments.
    *)
    (match analyze_reqs ~g ~ctx s c wl reqs with
    | _ :: _ as l ->
      let new_conf =
        List.fold l ~init:c ~f:(fun new_c (xi_id, arg_id) ->
            Sub.apply_diff (true, xi_id, arg_id) new_c)
      in
      G.add_next_candidate ~origin:c s new_conf
    | [] -> ())
  | _ -> analyze_witness_list ~ctx s c wl
;;
