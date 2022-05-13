open Base
open Term
open Utils
open Specifications
open Option.Let_syntax

(* ============================================================================================= *)
(*                      TYPE DEFINITIONS AND UTILS                                               *)
(* ============================================================================================= *)

(*
  A pattern in a PMRS rewrite rule is limited to matching
  terms of the form "Constructor_name(arguments)" where each argument is a term,
  and the Constructor_name is the name of a valid datatype constructor.
*)

(**
  A PMRS rewrite rule [(v, args, pattern, rhs)] is the rewrite rule
  v(args,pattern) -> rhs.
  [v] which is a non-terminal (a recursive function in the OCaml code),
  [args] is a possibly empty list of arguments,
  [pattern] is an optional pattern,
  [rhs] is the production of the rule.
*)
type rewrite_rule = variable * variable list * pattern option * term

(**
  A toplevel function is a triple of a variable (the variable representing
  the function itself), a list of variables (the arguments of the function) and
  a term (the body of the function).
*)
type top_function = variable * variable list * term

type t =
  { pvar : Variable.t
        (**
    The main function symbol, storing general information about the PMRS.
    The specification associated to the PMRS may be found using:
    [get_spec pvar]
    The type of the PMRS (the main function of the group of mutually recursive
    functions) is stored as the type of pvar.
  *)
  ; pinput_typ : RType.t list
        (**
    The input type(s). For now, it should be a singleton list.
    The input type is only the type of the recursively-typed argument of the PMRS, not the parameters.
  *)
  ; poutput_typ : RType.t (** Output type and optional invariant on output of function. *)
  ; pargs : variable list (** Parameter arguments.  *)
  ; psyntobjs : VarSet.t (** The unknowns to synthesize.*)
  ; prules : rewrite_rule IntMap.t (** The rules of the PMRS. *)
  ; pnon_terminals : VarSet.t (** Non-terminals of the PMRS. *)
  ; pmain_symb : variable (** The main symbol of the PMRS. *)
  ; porder : int (** The order of the PMRS (mostly useless for now). *)
  ; plogic : SmtLogic.logic_info
  }

(** Function contexts:
    - all PMRS in the file, indexed by the function variable id.
    - all the nonterminals, indexed by the function variable id.
*)
module Functions = struct
  type ctx =
    { globals : (int, t) Hashtbl.t
    ; nonterminals : (int, int) Hashtbl.t
    }

  let create () =
    { globals = Hashtbl.create (module Int); nonterminals = Hashtbl.create (module Int) }
  ;;

  let clear ctx =
    Hashtbl.clear ctx.globals;
    Hashtbl.clear ctx.nonterminals
  ;;

  let copy ctx =
    { globals = Hashtbl.copy ctx.globals; nonterminals = Hashtbl.copy ctx.nonterminals }
  ;;

  let register_global ctx (p : t) = Hashtbl.set ctx.globals ~key:p.pvar.vid ~data:p
  let find_global ctx (id : int) = Hashtbl.find ctx.globals id

  (** Find a PMRS by name. The name of a PMRS is the name of its pvar.  *)
  let find_by_name ctx (pmrs_name : string) : variable option =
    let matches =
      Hashtbl.filter ~f:(fun p -> String.(p.pvar.vname = pmrs_name)) ctx.globals
    in
    Option.map ~f:(fun (_, p) -> p.pvar) (Hashtbl.choose matches)
  ;;

  let register_nonterminal ctx (id : int) (pmrsid : t) =
    Hashtbl.set ctx.nonterminals ~key:id ~data:pmrsid.pvar.vid
  ;;

  let find_nonterminal ctx (id : int) =
    Option.(Hashtbl.find ctx.nonterminals id >>= fun pid -> Hashtbl.find ctx.globals pid)
  ;;

  (** Find a non-terminal (represented by a variable) from its name. *)
  let find_nonterminal_by_name ctx (name : string) : variable option =
    let r = ref None in
    Hashtbl.iter ctx.nonterminals ~f:(fun pid ->
        match Hashtbl.find ctx.globals pid with
        | Some t ->
          (match VarSet.find_by_name t.pnon_terminals name with
          | Some x -> r := Some x
          | None -> ())
        | None -> ());
    !r
  ;;

  let update ctx (p : t) =
    register_global ctx p;
    Set.iter p.pnon_terminals ~f:(fun v -> register_nonterminal ctx v.vid p)
  ;;
end

(**
  Generate the term that corresponds to the left hand side of a rewrite rule in
  a PMRS.
*)
let lhs ~(ctx : Context.t) (nt, args, pat, rhs) =
  let all_args =
    let args = List.map ~f:(mk_var ctx) args in
    match pat with
    | Some p -> args @ [ term_of_pattern ctx p ]
    | None -> args
  in
  let t, _ = infer_type ctx (mk_app ~pos:rhs.tpos (mk_var ctx nt) all_args) in
  t
;;

(* ============================================================================================= *)
(*                                 PRETTY PRINTING                                               *)
(* ============================================================================================= *)

let pp_rewrite_rule
    ~(ctx : Context.t)
    (frmt : Formatter.t)
    ((nt, vargs, pat, t) : rewrite_rule)
    : unit
  =
  Fmt.(
    pf
      frmt
      "@[<hov 2>@[<hov 2>%s %a %a  ⟹@] @;%a@]"
      nt.vname
      (list ~sep:sp (Variable.pp ctx))
      vargs
      (option (pp_pattern ctx))
      pat
      (box (pp_term ctx))
      t)
;;

let pp ~(ctx : Context.t) (frmt : Formatter.t) ?(short = false) (pmrs : t) : unit =
  let pp_rules frmt () =
    Map.iteri
      ~f:(fun ~key:_ ~data:(nt, args, pat, res) ->
        if Variable.(pmrs.pmain_symb = nt)
        then Fmt.(pf frmt "@[<v 2>‣ %a@]@;" (pp_rewrite_rule ~ctx) (nt, args, pat, res))
        else Fmt.(pf frmt "@[<v 2>  %a@]@;" (pp_rewrite_rule ~ctx) (nt, args, pat, res)))
      pmrs.prules
  in
  Fmt.(
    pf
      frmt
      "@[<hov> %s⟨%a⟩(%a): %a -> %a@;%a@;= @;@[<v 2>%a@]@]"
      pmrs.pvar.vname
      (VarSet.pp_var_names ctx)
      pmrs.psyntobjs
      (list ~sep:comma (Variable.pp ctx))
      pmrs.pargs
      (list ~sep:comma RType.pp)
      pmrs.pinput_typ
      RType.pp
      pmrs.poutput_typ
      (option (box (pp_spec ~ctx)))
      (Specifications.get_spec ~ctx pmrs.pvar)
      (if short then fun _ _ -> () else braces pp_rules)
      ())
;;

(**
  Pretty-print a PMRS as a set of OCaml functions.
*)
let pp_ocaml ~(ctx : Context.t) (frmt : Formatter.t) ?(short = false) (pmrs : t) : unit =
  let _ = short in
  let pp_term = pp_term ctx in
  let print_caml_def (frmt : Formatter.t) (nt, args, cases) =
    let pp_case f (opat, rhs) =
      match opat with
      | Some pat -> Fmt.(pf f "@[@[%a@] -> %a@]" (pp_pattern ctx) pat pp_term rhs)
      | None -> Fmt.(pf f "@[<missing pattern> ->@;%a@]" pp_term rhs)
    in
    match cases with
    | [] -> ()
    | [ (pat_opt, rhs) ] ->
      (match pat_opt with
      | Some pat ->
        Fmt.(
          pf
            frmt
            "@[<hov 2>%a @[%a_x@] %a@;@[%a _x %a@;@[%a -> %a@]@]@]"
            (styled (`Fg `Cyan) string)
            nt.vname
            (list ~sep:sp (Variable.pp ctx))
            args
            (styled (`Fg `Red) string)
            "="
            (styled (`Fg `Yellow) string)
            "match"
            (styled (`Fg `Yellow) string)
            "with"
            (pp_pattern ctx)
            pat
            pp_term
            rhs)
      | None ->
        Fmt.(
          pf
            frmt
            "@[<hov 2>%a @[%a@] %a@;%a@]"
            (styled (`Fg `Cyan) string)
            nt.vname
            (list ~sep:sp (Variable.pp ctx))
            args
            (styled (`Fg `Red) string)
            "="
            pp_term
            rhs))
    | _ :: _ ->
      Fmt.(
        pf
          frmt
          "@[%a %a%a@]@;<1 2>@[%a@;%a@]"
          (styled (`Fg `Cyan) string)
          nt.vname
          (list ~sep:sp (Variable.pp ctx))
          args
          (styled (`Fg `Red) string)
          "="
          (styled (`Fg `Yellow) string)
          "function"
          (list ~sep:(fun f () -> pf f "@;%a" (styled (`Fg `Yellow) string) "| ") pp_case)
          cases)
  in
  let functions =
    let f nt =
      let nt_rules =
        let f (nth, _, _, _) = Variable.(nth = nt) in
        Map.filter ~f pmrs.prules
      in
      let args, match_cases =
        let reconstr_cases (args, match_cases) (_, (_, args', pat, rhs)) =
          let pre_subst = List.zip_exn args' args in
          let substs = List.map ~f:(fun (x, y) -> mk_var ctx x, mk_var ctx y) pre_subst in
          args, match_cases @ [ pat, substitution substs rhs ]
        in
        match Map.to_alist nt_rules with
        | [] -> [], []
        | (_, (_, args, pat, rhs)) :: tl ->
          let init = args, [ pat, rhs ] in
          List.fold ~f:reconstr_cases ~init tl
      in
      nt, args, match_cases
    in
    List.map ~f (Set.elements pmrs.pnon_terminals)
  in
  match functions with
  | [] -> ()
  | hd :: tl ->
    Fmt.(pf frmt "@[%a %a@]@." (styled (`Fg `Red) string) "let rec" print_caml_def hd);
    List.iter tl ~f:(fun caml_def ->
        Fmt.(
          pf frmt "@[%a %a@]@." (styled (`Fg `Red) string) "and" print_caml_def caml_def))
;;

(* ============================================================================================= *)
(*                             BASIC PROPERTIES AND TYPE INFERENCE                               *)
(* ============================================================================================= *)

(** Update the order of the pmrs. *)
let update_order (p : t) : t =
  let order =
    let f ~key:_ ~data:(_, args, p, _) m =
      max m (List.length args + if Option.is_some p then 1 else 0)
    in
    Map.fold ~f ~init:0 p.prules
  in
  { p with porder = order }
;;

let set_logic_info_of_pmrs ~(ctx : Context.t) (p : t) : t =
  let f ~key:_ ~data:(_, _, _, rhs) logic_info =
    let operators = Analysis.operators_of rhs in
    let theory = SmtLogic.theory_of (type_of rhs) in
    SmtLogic.
      { theory = Smtlib.Logics.join_theories theory logic_info.theory
      ; linearity = logic_info.linearity && Set.for_all ~f:Operator.is_lia operators
      ; datatypes = SmtLogic.term_requires_datatype ~ctx rhs
      }
  in
  let li = Map.fold ~f ~init:SmtLogic.base_logic_info p.prules in
  { p with plogic = li }
;;

(** Clear the type information stored in the different components of the PMRS.
  Clears the type information of:
  - the local variables in each rule of the PMRS.
  - the pvar of the PMRS,
  - the pmain_symb of the PMRS,
  - the unknowns in the PMRS.
*)
let clear_pmrs_types ~(ctx : Context.t) (prog : t) : t =
  let rec clear_type_pat pat =
    match pat with
    | PatConstr (_, args) -> List.iter ~f:clear_type_pat args
    | PatVar v -> Variable.clear_type ctx v
    | PatTuple args -> List.iter ~f:clear_type_pat args
    | PatAny | PatConstant _ -> ()
  in
  let f_rule ~key:_ ~data:(nt, args, pat, body) : _ =
    List.iter ~f:(Variable.clear_type ctx) args;
    let _ = Option.map ~f:clear_type_pat pat in
    nt, args, pat, erase_term_type ctx body
  in
  let prules = Map.mapi ~f:f_rule prog.prules in
  Set.iter ~f:(Variable.clear_type ctx) (Set.union prog.pnon_terminals prog.psyntobjs);
  Variable.clear_type ctx prog.pvar;
  Variable.clear_type ctx prog.pmain_symb;
  { prog with
    prules
  ; pinput_typ = [ RType.get_fresh_tvar ctx.types ]
  ; poutput_typ = RType.get_fresh_tvar ctx.types
  }
;;

let infer_pmrs_types ~(ctx : Context.t) (prog : t) =
  let infer_aux ~key ~data:(nt, args, pat, body) (map, substs) =
    let t_body, c_body = infer_type ctx body in
    let t_head, c_head =
      let head_term =
        let t_args = List.map ~f:(mk_var ctx) args in
        match pat with
        | Some pattern -> mk_app (mk_var ctx nt) (t_args @ [ term_of_pattern ctx pattern ])
        | None -> mk_app (mk_var ctx nt) t_args
      in
      infer_type ctx head_term
    in
    let cur_loc = body.tpos in
    let c_rule = RType.merge_subs cur_loc c_body c_head in
    match RType.unify (RType.mkv (substs @ c_rule) @ [ t_head.ttyp, t_body.ttyp ]) with
    | Ok res ->
      ( Map.set map ~key ~data:(nt, args, pat, rewrite_types ctx (RType.mkv res) t_body)
      , res )
    | Error e ->
      Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
      Log.loc_fatal_errmsg
        cur_loc
        (Fmt.str
           "(%a) has type %a, expected type %a."
           (pp_term ctx)
           body
           RType.pp
           t_body.ttyp
           RType.pp
           t_head.ttyp)
  in
  let new_rules, new_subs =
    Map.fold prog.prules ~f:infer_aux ~init:(Map.empty (module Int), [])
  in
  match RType.unify (RType.mkv new_subs) with
  | Ok usubs ->
    Variable.update_var_types ctx (RType.mkv usubs);
    let typ_in, typ_out =
      RType.fun_typ_unpack (Variable.vtype_or_new ctx prog.pmain_symb)
    in
    Variable.update_var_types
      ctx
      [ Variable.vtype_or_new ctx prog.pvar, Variable.vtype_or_new ctx prog.pmain_symb ];
    (* Change types in the specification. *)
    (* Ensures. *)
    let _ =
      let%bind spec = get_spec ~ctx prog.pvar in
      let%map invariant =
        Option.map ~f:(fun ens -> first (infer_type ctx ens)) spec.ensures
      in
      Specifications.set_spec ~ctx prog.pvar { spec with ensures = Some invariant }
    in
    (* Requires *)
    let _ =
      let%bind spec = get_spec ~ctx prog.pvar in
      let%map invariant =
        Option.map ~f:(fun ens -> first (infer_type ctx ens)) spec.requires
      in
      (* Check that input of requires is same as input of function (without parameters). *)
      let req_t_in, _ = RType.fun_typ_unpack invariant.ttyp in
      let err_msg =
        Fmt.(
          str
            "Input type of @@requires of %s (%a) does not match input type of %s (%a)."
            prog.pvar.vname
            (list ~sep:ast RType.pp)
            req_t_in
            prog.pvar.vname
            (list ~sep:ast RType.pp)
            typ_in)
      in
      match List.zip req_t_in typ_in with
      | List.Or_unequal_lengths.Ok t ->
        (match RType.unify t with
        | Ok _ ->
          Specifications.set_spec ~ctx prog.pvar { spec with requires = Some invariant }
        | Error e ->
          Log.error_msg (Sexp.to_string_hum e);
          Log.error_msg err_msg;
          failwith err_msg)
      | List.Or_unequal_lengths.Unequal_lengths ->
        Log.error_msg err_msg;
        failwith err_msg
    in
    set_logic_info_of_pmrs
      ~ctx
      { prog with prules = new_rules; pinput_typ = typ_in; poutput_typ = typ_out }
  | Error e ->
    Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
    failwith "Type inference failed for pmrs."
;;

let unify_two_with_vartype_update
    ~(ctx : Context.t)
    ((theta, theta') : RType.t * RType.t)
    ((tau, tau') : RType.t * RType.t)
    : RType.substitution
  =
  let sb1 = RType.unify_one theta theta' in
  let sb2 = RType.unify_one tau tau' in
  match sb1, sb2 with
  | Ok sb1, Ok sb2 ->
    (match RType.unify (RType.mkv (sb1 @ sb2)) with
    | Ok sb' ->
      Term.Variable.update_var_types ctx (RType.mkv sb');
      sb'
    | Error e ->
      Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
      Log.error_msg "Could not unify θ and τ in problem definition.";
      Log.fatal ())
  | _ ->
    Log.error_msg
      (Fmt.str
         "repr has type %a, expected %a."
         RType.pp
         RType.(TFun (theta', tau'))
         RType.pp
         RType.(TFun (theta, tau)));
    Log.fatal ()
;;

let unify_one_with_update ~ctx (t, t') =
  let sb1 = RType.unify_one t t' in
  match sb1 with
  | Ok sb1 ->
    (match RType.unify (RType.mkv sb1) with
    | Ok sb' -> Term.Variable.update_var_types ctx (RType.mkv sb')
    | Error e ->
      Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
      Log.error_msg "Could not unify θ and τ in problem definition.";
      Log.fatal ())
  | _ ->
    Log.error_msg
      (Fmt.str "Match PMRS on type %a with %a impossible." RType.pp t' RType.pp t);
    Log.fatal ()
;;

let extract_rec_input_typ (prog : t) =
  match List.last prog.pinput_typ with
  | Some t -> t
  | None -> failwith "PMRS should have at least one input for recursion."
;;

(* ============================================================================================= *)
(*                             TRANSLATION FROM FUNCTION to PMRS and back                        *)
(* ============================================================================================= *)

let func_to_pmrs
    ~(ctx : Context.t)
    (f : Variable.t)
    (args : fpattern list)
    (body : Term.term)
  =
  let tin, tout =
    match Variable.vtype_or_new ctx f with
    | RType.TFun (tin, tout) -> tin, tout
    | _ -> failwith "Cannot make pmrs of non-function."
  in
  let pmain_symb, prules, pnon_terminals =
    match args, body with
    | [ FPatVar x ], { tkind = TVar x_v; _ } when Variable.(x = x_v) ->
      f, Map.singleton (module Int) 0 (f, [ x ], None, body), VarSet.singleton x
    | _ -> failwith "TODO: only identity supported by func_to_pmrs"
  in
  { pvar = f
  ; pinput_typ = [ tin ]
  ; poutput_typ = tout
  ; pargs = Set.elements (fpat_vars (FPatTup args))
  ; psyntobjs = VarSet.empty
  ; (* PMRS from a function cannot have unkowns. *)
    porder = 0
  ; pmain_symb
  ; prules
  ; pnon_terminals
  ; plogic = SmtLogic.base_logic_info
  }
;;

let build_match_cases
    ~(ctx : Context.t)
    (pmrs : t)
    _nont
    (vars : Variable.t list)
    (relevant_rules : rewrite_rule list)
    : (term * match_case list) option
  =
  let build_with matched_var rest_args =
    let rule_to_match_case (_, var_args, pat, body) =
      let non_pattern_matched_args = pmrs.pargs @ var_args in
      let case_body =
        let extra_param_args = List.map ~f:(Term.mk_var ctx) pmrs.pargs in
        let body' =
          let case f t =
            match t.tkind with
            | TApp ({ tkind = TVar fv; _ }, args) ->
              if Set.mem pmrs.pnon_terminals fv
              then (
                let args' = List.map ~f args in
                Some (mk_app (Term.mk_var ctx fv) (extra_param_args @ args')))
              else None
            | _ -> None
          in
          Term.transform ~case body
        in
        let sub =
          match
            List.map2
              ~f:(fun v x -> Term.mk_var ctx v, Term.mk_var ctx x)
              non_pattern_matched_args
              rest_args
          with
          | Ok zipped -> zipped
          | Unequal_lengths -> failwith "Unexpected."
        in
        substitution sub body'
      in
      Option.map ~f:(fun x -> x, case_body) pat
    in
    Option.map
      ~f:(fun l -> mk_var ctx matched_var, l)
      (all_or_none (List.map ~f:rule_to_match_case relevant_rules))
  in
  match List.last vars, List.drop_last vars with
  | Some x, Some rest -> build_with x rest
  | _ -> None
;;

let single_rule_case ~(ctx : Context.t) (vars : Variable.t list) (args, body) : term =
  let sub =
    match List.map2 ~f:(fun v x -> Term.mk_var ctx v, Term.mk_var ctx x) args vars with
    | Ok zipped -> zipped
    | Unequal_lengths -> failwith "Unexpected."
  in
  substitution sub body
;;

let vars_and_formals ~(ctx : Context.t) (pmrs : t) (fvar : variable) =
  let args_t, _ = RType.fun_typ_unpack (Variable.vtype_or_new ctx fvar) in
  List.map
    ~f:(fun rt ->
      let v =
        Variable.mk
          ctx
          ~t:(Some rt)
          (Alpha.fresh ctx.names ~s:("x" ^ String.drop_suffix fvar.vname 2))
      in
      v)
    (List.map ~f:(fun v -> Variable.vtype_or_new ctx v) pmrs.pargs @ args_t)
;;

let func_of_pmrs ~(ctx : Context.t) (pmrs : t) : function_descr list =
  (* Define recursive functions. *)
  let fun_of_nont (nont : variable) : function_descr option =
    let vars = vars_and_formals ~ctx pmrs nont in
    let maybe_body =
      let relevant_rules =
        Map.data (Map.filter ~f:(fun (k, _, _, _) -> k.vid = nont.vid) pmrs.prules)
      in
      let all_pattern_matching =
        List.for_all relevant_rules ~f:(fun (_, _, pat, _) -> Option.is_some pat)
      in
      if all_pattern_matching
      then (
        match build_match_cases ~ctx pmrs nont vars relevant_rules with
        | Some (x, match_cases) -> Some (mk_match ctx x match_cases)
        | None ->
          (match relevant_rules with
          | [ (_, args, _, body) ] -> Some (single_rule_case ~ctx vars (args, body))
          | _ -> None))
      else (
        match relevant_rules with
        | [ (_, args, _, body) ] -> Some (single_rule_case ~ctx vars (args, body))
        | _ -> None)
    in
    Option.map maybe_body ~f:(fun body ->
        { f_var = nont; f_args = List.map ~f:mk_pat_var vars; f_body = body })
  in
  List.filter_map ~f:fun_of_nont (Set.elements pmrs.pnon_terminals)
;;

(* ============================================================================================= *)
(*                                           UTILS FOR PMRS                                      *)
(* ============================================================================================= *)

(**
  inverted_rule_lookup searches for rules whose rhs match (func args), and return
  a map from rule id to the lhs of the rules matching (func args), with the appropriate
  substitutions performed.
*)
let inverted_rule_lookup
    ?(boundvars = VarSet.empty)
    ~(ctx : Context.t)
    rules
    (func : term)
    (args : term list)
  =
  let list_matching l =
    let m =
      List.map l ~f:(fun (rhs_arg, arg) ->
          Matching.matches ~ctx ~boundvars ~pattern:rhs_arg arg)
    in
    let merge_subs ~key:_ s =
      match s with
      | `Both (s1, s2) -> if Terms.(equal s1 s2) then Some s1 else failwith "x"
      | `Left s1 -> Some s1
      | `Right s2 -> Some s2
    in
    try
      let fold_f subs maybe_subs =
        match maybe_subs with
        | Some subs' -> Map.merge subs' subs ~f:merge_subs
        | None -> failwith "l"
      in
      Some (List.fold m ~init:(Map.empty (module Variable)) ~f:fold_f)
    with
    | _ -> None
  in
  let filter (nt, rule_args, rule_pat, rule_rhs) =
    let lhs_term substs =
      let t = lhs ~ctx (nt, rule_args, rule_pat, rule_rhs) in
      substitution (Terms.substs_of_alist (Map.to_alist substs)) t
    in
    match rule_rhs.tkind with
    | TApp (rhs_func, rhs_args) ->
      if Terms.(equal rhs_func func)
      then (
        match List.zip rhs_args args with
        | Ok l -> Option.map ~f:lhs_term (list_matching l)
        | _ -> None)
      else None
    | _ -> None
  in
  Map.filter_map ~f:filter rules
;;

(**
  Apply a substitution to all the right hand side of the PMRS rules.
*)
let subst_rule_rhs ~(ctx : Context.t) ~(p : t) (substs : (term * term) list) =
  let rules' =
    let f (nt, args, pat, body) =
      let body' = substitution substs body in
      let body'', _ = infer_type ctx body' in
      nt, args, pat, body''
    in
    Map.map ~f p.prules
  in
  { p with prules = rules' }
;;

(**
  Given an input PMRS, returns a list of PMRS that this PMRS depends on.
  *)
let depends ~(glob : Functions.ctx) ~(ctx : Context.t) (p : t) : t list =
  let f (_, _, _, rule_body) =
    let vars = Analysis.free_variables ~ctx rule_body in
    List.filter_map ~f:(fun v -> Hashtbl.find glob.globals v.vid) (Set.elements vars)
  in
  List.dedup_and_sort
    ~compare:(fun p1 p2 -> String.compare p1.pvar.vname p2.pvar.vname)
    (List.concat_map ~f (Map.data p.prules))
;;

(**
  Updates and returns the output type of a PMRS.
*)
let update_output_type ~(ctx : Context.t) (p : t) : t =
  let ot = snd (RType.fun_typ_unpack (Variable.vtype_or_new ctx p.pvar)) in
  { p with poutput_typ = ot }
;;
