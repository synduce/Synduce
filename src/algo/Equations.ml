open Base
open Lang
open Lang.Term
open AState
open Syguslib.Sygus
open SygusInterface
open Utils
open Either
module SmtI = SmtInterface

type equation = term * term option * term * term

let pp_equation (f : Formatter.t) (orig, inv, lhs, rhs : equation) =
  match inv with
  | Some inv -> Fmt.(pf f "@[<hov 2>E(%a) := @;@[<hov 2>%a %a@;@[%a@;%a@;%a@]@]@]"
                       (styled `Italic  pp_term) orig
                       pp_term inv
                       (styled (`Fg `Red) string) "=>"
                       pp_term lhs
                       (styled (`Fg `Red) string) "="
                       pp_term rhs)
  | None -> Fmt.(pf f "@[<hov 2>E(%a) := @;@[<hov 2>%a %a %a@]@]"
                   (styled `Italic pp_term) orig
                   pp_term lhs
                   (styled (`Fg `Red) string) "="
                   pp_term rhs)


(* ============================================================================================= *)
(*                        PROJECTION : OPTIMIZATION FOR TUPLES                                   *)
(* ============================================================================================= *)

let mk_projs (tin : RType.t) (tl : RType.t list) (xi : Variable.t) =
  let f i t =
    Variable.mk ~t:(Some RType.(TFun(tin, t))) (xi.vname^(Int.to_string i))
  in List.mapi ~f tl


let projection_eqns (lhs : term) (rhs : term) =
  match lhs.tkind, rhs.tkind with
  | TTup lhs_tl, TTup rhs_tl -> List.map ~f:(fun (r,l) -> r,l) (List.zip_exn lhs_tl rhs_tl)
  | _ -> [lhs, rhs]

let invar invariants lhs_e rhs_e =
  let f inv_expr =
    not (Set.is_empty
           (Set.inter
              (Set.union (Analysis.free_variables lhs_e) (Analysis.free_variables rhs_e))
              (Analysis.free_variables inv_expr)))
  in
  let conjs = List.filter ~f (Set.elements invariants) in
  mk_assoc Binop.And conjs
let proj_and_detuple_eqns (projections : (int, variable list, Int.comparator_witness) Map.t)
    (eqns : equation list) =
  let apply_p = Analysis.apply_projections projections in
  let f (t, pre, lhs, rhs) =
    let lhs' = apply_p lhs and rhs' = apply_p rhs in
    let eqs = projection_eqns lhs' rhs' in
    List.map ~f:(fun (_l, _r) ->  t, pre, _l, _r) eqs
  in
  List.concat (List.map ~f eqns)


let proj_unknowns (unknowns : VarSet.t) =
  let unknowns_projs, new_unknowns =
    let f (l, vs) xi =
      match Variable.vtype_or_new xi with
      | RType.TFun(tin, TTup tl) ->
        let new_vs = mk_projs tin tl xi in
        l @ [xi, Some new_vs], Set.union vs (VarSet.of_list new_vs)
      | _ ->
        l @ [xi, None], Set.add vs xi
    in
    List.fold ~f ~init:([], VarSet.empty) (Set.elements unknowns)
  in
  let proj_map =
    let mmap =
      Map.of_alist (module Int)
        (List.map ~f:(fun (x, p) -> x.vid, p)
           (* Only select relevant xi for projection *)
           (List.filter_map
              ~f:(fun (_x, _o) -> match _o with Some o -> Some (_x, o) | None -> None)
              unknowns_projs))
    in
    match mmap with
    | `Ok x -> x
    | `Duplicate_key _ -> failwith "Unexpected error while projecting."
  in
  new_unknowns, proj_map


(* ============================================================================================= *)
(*                               BUILDING SYSTEMS OF EQUATIONS                                   *)
(* ============================================================================================= *)

let check_equation ~(p : psi_def) (_, pre, lhs, rhs : equation) : bool =
  (match Expand.nonreduced_terms_all p lhs, Expand.nonreduced_terms_all p rhs with
   | [], [] -> true
   | _ -> false) &&
  (match pre with
   | None -> true
   | Some t -> match Expand.nonreduced_terms_all p t with | [] -> true | _ -> false)


(**
   Compute the left hand side of an equation of p from term t.
   The result is a maximally reduced term with some applicative
   terms of the form (p.orig x) where x is a variable.
*)
let compute_lhs p t =
  let t' = Reduce.reduce_pmrs p.repr t in
  let r_t = Expand.replace_rhs_of_main p p.repr t' in
  let subst_params =
    let l = List.zip_exn p.orig.pargs p.target.pargs in
    List.map l ~f:(fun (v1, v2) -> mk_var v1, mk_var v2)
  in
  let f_r_t = Reduce.reduce_pmrs p.orig r_t in
  let final = substitution subst_params f_r_t in
  Expand.replace_rhs_of_mains p (Reduce.reduce_term final)


let remap_rec_calls p t =
  let g = p.target in
  let t' = Expand.replace_rhs_of_main p g t in
  let f a _t =
    match _t.tkind with
    | TApp({tkind = TVar x; _}, args) ->
      (if a && Variable.equal x g.pmain_symb then
         (match args with
          | [arg] -> First  (compute_lhs p arg)
          | _ -> Second a)
       else if Set.mem g.pparams x then
         Second true
       else Second a
      )
    | _ -> Second a
  in
  let res = rewrite_accum ~init:false ~f t' in
  if Term.term_equal res t' then t (* Don't revert step taken before *)
  else res


let compute_rhs_with_replacing p t =
  let g = p.target in
  let custom_reduce x =
    let one_step t0 =
      let rstep = ref false in
      let rewrite_rule _t =
        match _t.tkind with
        | TApp({tkind=(TVar(f)); _}, fargs) ->
          (match Reduce.rule_lookup g.prules f fargs with
           | [] -> None
           | hd :: _ ->
             let hd' = remap_rec_calls p hd in
             rstep := true;
             Some hd')
        (* Replace recursive calls to g by calls to f circ g,
           if recursive call appear under unknown. *)
        | _ -> None
      in
      let t0' = rewrite_top_down rewrite_rule t0 in
      t0', !rstep
    in
    let steps = ref 0 in
    let rec apply_until_irreducible t =
      Int.incr steps;
      let t', reduced =  one_step t in
      if reduced then apply_until_irreducible t' else t'
    in
    apply_until_irreducible x
  in
  let app_t = mk_app (mk_var g.pmain_symb) [t] in
  let t' = Reduce.reduce_term (custom_reduce app_t) in
  let _res = Expand.replace_rhs_of_mains p t' in
  _res


let compute_rhs ?(force_replace_off=false) p t =
  if !Config.replace_recursion && (not force_replace_off) then
    compute_rhs_with_replacing p t
  else
    let res = Expand.replace_rhs_of_mains p
        (Reduce.reduce_term (Reduce.reduce_pmrs p.target t))
    in res


let make ?(force_replace_off = false) ~(p : psi_def) (tset : TermSet.t) : equation list =
  let eqns =
    let fold_f eqns t =
      let lhs = compute_lhs p t in
      let rhs = compute_rhs ~force_replace_off p t in
      eqns @ [t, lhs, rhs]
    in
    Set.fold ~init:[] ~f:fold_f tset
  in
  let all_subs, invariants =
    Expand.subst_recursive_calls p
      (List.concat (List.map ~f:(fun (_, lhs, rhs) -> [lhs; rhs]) eqns))
  in
  (if Set.length invariants > 0 then
     Log.verbose Fmt.(fun frmt () -> pf frmt "Invariants:@[<hov 2>%a@]"
                         (list ~sep:comma pp_term) (Set.elements invariants))
   else Log.verbose_msg "No invariants.");
  let pure_eqns =
    let f (t, lhs, rhs) =
      let applic x = substitution all_subs (Reduce.reduce_term (substitution all_subs x)) in
      let lhs' = Reduce.reduce_term (applic lhs)
      and rhs' = Reduce.reduce_term (applic rhs) in
      let projs = projection_eqns lhs' rhs' in
      List.map ~f:(fun (lhs,rhs) -> t, invar invariants lhs rhs, lhs, rhs) projs
    in
    List.concat (List.map ~f eqns)
  in
  Log.verbose
    (fun f () -> Fmt.(pf f "Equations > make (%i) @." (Set.length tset);
                      List.iter ~f:(fun eqn -> Fmt.pf f "@[%a@]@." pp_equation eqn) pure_eqns));

  match List.find ~f:(fun eq -> not (check_equation ~p eq)) pure_eqns with
  | Some not_pure ->
    Log.error_msg Fmt.(str "Not pure: %a" pp_equation not_pure);
    failwith "Equation not pure."
  | None ->  pure_eqns


let revert_projs
    (orig_xi : VarSet.t)
    (projections : (int, variable list, Int.comparator_witness) Map.t)
    (soln : (string * (variable list) * term) list) :
  (string * variable list * term) list =
  (* Helper functions *)
  let find_soln s =
    List.find_exn ~f:(fun (s', _, _) -> String.equal s.vname s') soln
  in
  let join_bodies main_args first_body rest =
    let f accum (_, args, body) =
      let subst =
        match List.zip args main_args with
        | Ok l -> List.map l ~f:(fun (v1,v2) -> mk_var v1, mk_var v2)
        | Unequal_lengths -> failwith "Projections should have same number of arguments."
      in
      accum @ [substitution subst body]
    in
    let tuple_elts =  List.fold ~f ~init:[first_body] rest in
    mk_tup tuple_elts
  in
  (* Helper sets *)
  let all_proj_names, xi_projected =
    let x0 = Map.to_alist projections in
    let x1 =
      List.concat (List.map ~f:(fun (id, l) -> List.map ~f:(fun e -> id, e) l) x0)
    in
    List.map ~f:(fun (_, v) -> v.vname) x1,
    VarSet.of_list (List.filter_map ~f:(fun (id, _) -> VarSet.find_by_id orig_xi id) x1)
  in
  let _, rest =
    let f (s, _, _) =
      List.mem all_proj_names ~equal:String.equal s
    in
    List.partition_tf ~f soln
  in
  (* Build for each xi projected *)
  let new_xi_solns =
    let f xi =
      let vls = Map.find_exn projections  xi.vid in
      let solns = List.map ~f:find_soln vls in
      match solns with
      | [] -> failwith "revert_projs : failed to find an expected solution."
      | [_, args, body] -> xi.vname, args, body (* This should not happen though. *)
      | (_, args1, body1) :: tl -> xi.vname, args1, join_bodies args1 body1 tl
    in
    List.map ~f (Set.elements xi_projected)
  in
  rest @ new_xi_solns


(* ============================================================================================= *)
(*                               SOLVING SYSTEMS OF EQUATIONS                                    *)
(* ============================================================================================= *)
let pp_soln (f : Formatter.t) soln =
  Fmt.(list ~sep:comma (fun fmrt (s, args, bod) ->
      match args with
      | [] -> pf fmrt "@[<hov 2>@[%s@] = @[%a@]@]"
                s pp_term bod
      | _ -> pf fmrt "@[<hov 2>@[%s(%a)@] = @[%a@]@]"
               s (list ~sep:comma Variable.pp) args pp_term bod))
    f soln


let synthfuns_of_unknowns ?(bools = false) ?(ops = OpSet.empty) (unknowns : VarSet.t) =
  let xi_formals (xi : variable) : sorted_var list * sygus_sort =
    match Variable.vtype_or_new xi with
    | RType.TFun(TTup(targs), tres) -> sorted_vars_of_types targs, sort_of_rtype tres
    | RType.TFun(targ, tres) -> sorted_vars_of_types [targ], sort_of_rtype tres
    | t -> [], sort_of_rtype t
  in
  let f xi =
    let args, ret_sort = xi_formals xi in
    let grammar = Grammars.generate_grammar ~bools ops args ret_sort in
    CSynthFun (xi.vname, args, ret_sort, grammar)
  in
  List.map ~f (Set.elements unknowns)


let constraints_of_eqns (eqns : equation list) : command list =
  let detupled_equations =
    let f (_, pre, lhs, rhs) =
      let eqs = projection_eqns lhs rhs in
      List.map ~f:(fun (_l, _r) -> pre, _l, _r) eqs
    in
    List.concat (List.map ~f eqns)
  in
  let eqn_to_constraint (pre, lhs, rhs) =
    match pre with
    | Some precondition ->
      CConstraint (
        SyApp(IdSimple "or",
              [
                SyApp(IdSimple "not", [sygus_of_term precondition]);
                SyApp(IdSimple "=", [sygus_of_term lhs; sygus_of_term rhs])
              ]))
    | None -> CConstraint (SyApp(IdSimple "=", [sygus_of_term lhs; sygus_of_term rhs]))
  in
  List.map ~f:eqn_to_constraint detupled_equations


let solve_eqns (unknowns : VarSet.t) (eqns : equation list) =
  let free_vars, all_operators, has_ite =
    let f (fvs, ops, hi) (_, _, lhs, rhs) =
      VarSet.union_list [fvs; Analysis.free_variables lhs; Analysis.free_variables rhs],
      Set.union ops (Set.union (Grammars.operators_of lhs) (Grammars.operators_of rhs)),
      hi || (Analysis.has_ite lhs) || (Analysis.has_ite rhs)
    in
    let fvs, ops, hi = List.fold eqns ~f ~init:(VarSet.empty, Set.empty (module Operator), false) in
    Set.diff fvs unknowns, ops, hi
  in
  (* Commands *)
  let set_logic = CSetLogic(Grammars.logic_of_operator all_operators) in
  let synth_objs = synthfuns_of_unknowns ~bools:has_ite ~ops:all_operators unknowns in
  let sort_decls = declare_sorts_of_vars free_vars in
  let var_decls = List.map ~f:declaration_of_var (Set.elements free_vars) in
  let constraints = constraints_of_eqns eqns in
  let extra_defs =
    (if Set.mem all_operators (Binary Max) then [max_definition] else []) @
    (if Set.mem all_operators (Binary Min) then [min_definition] else [])
  in
  let commands =
    set_logic :: (extra_defs @ sort_decls @ synth_objs @ var_decls @ constraints @ [CCheckSynth])
  in
  (* Call the solver. *)
  let handle_response (resp : solver_response) =
    let parse_synth_fun (fname, fargs, _, fbody) =
      let args =
        let f (varname, sort) = Variable.mk ~t:(rtype_of_sort sort) varname in
        List.map ~f fargs
      in
      let local_vars = VarSet.of_list args in
      let body, _ = infer_type (term_of_sygus (VarSet.to_env local_vars) fbody) in
      fname, args, body
    in
    match resp with
    | RSuccess (resps) ->
      let soln = List.map ~f:parse_synth_fun resps in
      resp, Some soln

    | RInfeasible -> RInfeasible, None

    | RFail -> RFail, None

    | RUnknown -> RUnknown, None
  in
  match Syguslib.Solvers.SygusSolver.solve_commands commands with
  | Some resp -> handle_response resp
  | None -> RFail, None


(* Solve the trivial equations first, avoiding the overhead from the
   sygus solver.
*)
let solve_constant_eqns (unknowns : VarSet.t) (eqns : equation list) =
  let constant_soln, other_eqns =
    let f (t, inv, lhs, rhs) =
      match rhs.tkind with
      | TVar x when Set.mem unknowns x ->
        (* TODO check that lhs is a constant term. (Should be the case if wf) *)
        Either.first (x, lhs)
      | _ -> Either.Second (t, inv, lhs, rhs)
    in
    List.partition_map ~f eqns
  in
  let resolved = VarSet.of_list (List.map ~f:Utils.first constant_soln) in
  let new_eqns =
    let substs = List.map ~f:(fun (x, lhs) -> mk_var x, lhs) constant_soln in
    List.map other_eqns
      ~f:(fun (t, inv, lhs, rhs) -> t, inv, substitution substs lhs, substitution substs rhs)
  in
  let partial_soln =
    List.map ~f:(fun (x, lhs) -> x.vname, [], lhs) constant_soln
  in
  if List.length partial_soln > 0 then
    Log.debug_msg Fmt.(str "Constant:@;@[<hov 2>%a@]" pp_soln partial_soln);
  partial_soln,
  Set.diff unknowns resolved,
  new_eqns


let solve_syntactic_definitions (unknowns : VarSet.t) (eqns : equation list) =
  (* Is lhs, args a full definition of the function? *)
  let ok_args lhs args =
    let argv =  List.map args ~f:ext_var_or_none in
    let argset = VarSet.of_list (List.concat (List.filter_opt argv)) in
    if List.for_all ~f:Option.is_some argv &&
       (Set.is_empty (Set.diff (Analysis.free_variables lhs) argset))
    then
      let args = List.filter_opt argv in
      (if List.length (List.concat args) = Set.length argset then
         Some args
       else None)
    else
      None
  in
  let mk_lam lhs args =
    let pre_subst =
      List.map args
        ~f:(fun arg_tuple ->
            let t =
              match arg_tuple with
              | [v] -> Variable.vtype_or_new v
              | l -> RType.TTup (List.map ~f:Variable.vtype_or_new l)
            in
            let v = Variable.mk ~t:(Some t) (Alpha.fresh "x") in
            match arg_tuple with
            | [arg] -> v, [mk_var arg, mk_var v]
            | l -> v, List.mapi l ~f:(fun i arg -> mk_var arg, mk_sel (mk_var v) i)
          )
    in
    let new_args, subst = List.unzip pre_subst in
    new_args, Reduce.reduce_term (substitution (List.concat subst) lhs)
  in
  let full_defs, other_eqns =
    let f (t, inv, lhs, rhs) =
      match inv, rhs.tkind with
      | None, TApp({tkind=TVar x; _}, args) when Set.mem unknowns x ->
        (match ok_args lhs args with
         | Some argv ->
           let lam_args, lam_body = mk_lam lhs argv in
           Either.First (x, (lam_args, lam_body))
         | None ->
           Either.Second (t, inv, lhs, rhs))

      | _ -> Either.Second (t, inv, lhs, rhs)
    in
    List.partition_map ~f eqns
  in
  let resolved = VarSet.of_list (List.map ~f:Utils.first full_defs) in
  let new_eqns =
    let substs =
      List.map full_defs
        ~f:(fun (x, (lhs_args, lhs_body)) ->
            let t, _ = infer_type (mk_fun (List.map ~f:(fun x -> PatVar x) lhs_args) lhs_body) in
            mk_var x, t)
    in
    List.map other_eqns
      ~f:(fun (t, inv, lhs, rhs) ->
          let new_lhs = Reduce.reduce_term (substitution substs lhs) in
          let new_rhs = Reduce.reduce_term (substitution substs rhs) in
          t, inv, new_lhs, new_rhs)
  in
  let partial_soln =
    List.map
      ~f:(fun (x, (lhs_args, lhs_body)) -> x.vname, lhs_args, lhs_body)
      full_defs
  in
  if List.length partial_soln > 0 then
    Log.debug_msg Fmt.(str "Syntactic definition:@;@[<hov 2>%a@]" pp_soln partial_soln);
  partial_soln,
  Set.diff unknowns resolved,
  new_eqns



let split_solve partial_soln (unknowns : VarSet.t) (eqns : equation list) =
  (* If an unknown depends only on itself, it can be split from the rest *)
  let split_eqn_systems =
    let f (l, u, e) xi =
      (* Separate in set of equation where u appears and rest *)
      let eqn_u, rest =
        List.partition_tf e
          ~f:(fun (_, _, lhs, rhs) ->
              let fv = Set.union (Analysis.free_variables lhs)
                  (Analysis.free_variables rhs)
              in Set.mem fv xi)
      in
      let eqn_only_u, eqn_u =
        List.partition_tf eqn_u
          ~f:(fun (_, _, lhs, rhs) ->
              let fv = Set.union (Analysis.free_variables lhs)
                  (Analysis.free_variables rhs)
              in
              Set.is_empty (Set.inter fv (Set.diff unknowns (VarSet.singleton xi))))
      in
      match eqn_u with
      | [] ->
        Log.debug_msg Fmt.(str "Solve for %s independently." xi.vname);
        l @ [VarSet.singleton xi, eqn_only_u], u, rest
      | _ -> l, Set.add u xi, e
    in
    let sl, u , e =
      List.fold (Set.elements unknowns) ~f ~init:([], VarSet.empty, eqns)
    in sl @ [u,e]
  in
  let combine prev_sol new_response =
    match prev_sol, new_response with
    | Some soln, (resp, Some soln') ->
      Log.debug_msg Fmt.(str "Partial solution:@;@[<hov 2>%a@]" pp_soln soln');
      resp, Some (soln @ soln')
    | _, (resp, None) -> resp, None
    | None, (resp, _) -> resp, None
  in
  let solve_eqn_aux prev_resp prev_sol u e =
    if Set.length u > 0 then combine prev_sol (solve_eqns u e)
    else prev_resp, prev_sol
  in
  List.fold split_eqn_systems
    ~init:(RSuccess [], Some partial_soln)
    ~f:(fun (prev_resp, prev_sol) (u, e) -> solve_eqn_aux prev_resp prev_sol u e)


let solve_stratified (unknowns : VarSet.t) (eqns : equation list) =
  let psol, u, e =
    if !Config.syndef_on then
      let c_soln, no_c_unknowns, no_c_eqns =
      solve_constant_eqns unknowns eqns
      in
      let partial_soln', new_unknowns, new_eqns =
        solve_syntactic_definitions no_c_unknowns no_c_eqns
      in
      c_soln @ partial_soln', new_unknowns, new_eqns
    else
      [], unknowns, eqns
  in
  if !Config.split_solve_on then
    split_solve psol u e
  else
    match solve_eqns u e with
    | resp, Some soln -> resp, Some (psol @ soln)
    | resp, None -> resp, None


let solve ~(p : psi_def) (eqns : equation list) =
  let unknowns = p.target.pparams in
  let soln_final =
    if !Config.detupling_on then
      let new_unknowns, projections = proj_unknowns p.target.pparams in
      let new_eqns = proj_and_detuple_eqns projections eqns in
      match solve_stratified new_unknowns new_eqns with
      | resp, Some soln0 ->
        let soln =
          if Map.length projections > 0 then
            revert_projs unknowns projections soln0
          else soln0
        in resp, Some soln
      | resp, None -> resp, None
    else
      solve_stratified unknowns eqns
  in
  (match soln_final with
   | _, Some soln ->
     Utils.Log.debug_msg
       Fmt.(str "@[<hov 2>Solution found: @;%a" pp_soln soln);
   | _ -> ());
  soln_final