open Analysis
open Base
open Term
open Utils
open Specifications

(* ============================================================================================= *)
(*                      TYPE DEFINITIONS AND UTILS                                               *)
(* ============================================================================================= *)

type pattern = string * term list

type rewrite_rule = variable * variable list * pattern option * term

type top_function = variable * variable list * term

type t = {
  pvar : Variable.t;  (** The main function symbol *)
  pinput_typ : RType.t list;
      (**
    The input type(s). For now, it should be a singleton list.
    The input type is only the type of the recursively-typed argument of the PMRS, not the parameters.
  *)
  pspec : spec;
  (* A specification for the PMRS, in the form of optional requires and ensures clauses.
   *)
  poutput_typ : RType.t;  (** Output type and optional invariant on output of function. *)
  pargs : variable list;  (** Parameter arguments.  *)
  psyntobjs : VarSet.t;  (** The unknowns to synthesize.*)
  prules : rewrite_rule IntMap.t;  (** The rules of the PMRS. *)
  pnon_terminals : VarSet.t;  (** Non-terminals of the PMRS. *)
  pmain_symb : variable;  (** The main symbol of the PMRS. *)
  porder : int;  (** The order of the PMRS (mostly useless for now). *)
}

(* Type shortcuts *)
type 'a xresult = ('a, (string * Sexp.t) list) Result.t

type 'a sresult = ('a, (string * term) list) Result.t

type variables = variable Map.M(String).t

(** Table of all the PMRS in the file, indexed by the function
    variable id.
*)
let _globals : (int, t) Hashtbl.t = Hashtbl.create (module Int)

let find_by_name (pmrs_name : string) =
  let matches = Hashtbl.filter ~f:(fun p -> String.(p.pvar.vname = pmrs_name)) _globals in
  Option.map ~f:(fun (_, p) -> p.pvar) (Hashtbl.choose matches)

(** Mapping nonterminals to the PMRS they belong to, index by the nonterminal
    variable id.
*)
let _nonterminals : (int, t) Hashtbl.t = Hashtbl.create (module Int)

let lhs (nt, args, pat, rhs) =
  let all_args =
    let args = List.map ~f:mk_var args in
    match pat with Some (c, pat_args) -> args @ [ mk_data c pat_args ] | None -> args
  in
  let t, _ = infer_type (mk_app ~pos:rhs.tpos (mk_var nt) all_args) in
  t

(* ============================================================================================= *)
(*                                 PRETTY PRINTING                                               *)
(* ============================================================================================= *)
let pp_pattern (frmt : Formatter.t) ((t, args) : pattern) : unit =
  if List.length args = 0 then Fmt.(pf frmt "%a" (styled `Italic string) t)
  else Fmt.(pf frmt "%a(%a)" (styled `Italic string) t (list ~sep:comma Term.pp_term) args)

let pp_rewrite_rule (frmt : Formatter.t) ((nt, vargs, pat, t) : rewrite_rule) : unit =
  Fmt.(
    pf frmt "@[<hov 2>@[<hov 2>%s %a %a  ⟹@] @;%a@]" nt.vname (list ~sep:sp Variable.pp) vargs
      (option pp_pattern) pat (box pp_term) t)

let pp (frmt : Formatter.t) (pmrs : t) : unit =
  let pp_rules frmt () =
    Map.iteri
      ~f:(fun ~key:_ ~data:(nt, args, pat, res) ->
        if Variable.(pmrs.pmain_symb = nt) then
          Fmt.(pf frmt "@[<v 2>‣ %a@]@;" pp_rewrite_rule (nt, args, pat, res))
        else Fmt.(pf frmt "@[<v 2>  %a@]@;" pp_rewrite_rule (nt, args, pat, res)))
      pmrs.prules
  in
  Fmt.(
    pf frmt "%s⟨%a⟩(%a): %a -> %a@;%a@;= @;@[<v 2>{@;%a@;}@]" pmrs.pvar.vname
      VarSet.pp_var_names pmrs.psyntobjs (list Variable.pp) pmrs.pargs (list ~sep:comma RType.pp)
      pmrs.pinput_typ RType.pp pmrs.poutput_typ (box pp_spec) pmrs.pspec pp_rules ())

let pp_ocaml (frmt : Formatter.t) (pmrs : t) : unit =
  let print_caml_def (frmt : Formatter.t) (nt, args, cases) =
    let pp_case f (opat, rhs) =
      match opat with
      | Some pat -> Fmt.(pf f "@[@[%a@] -> %a@]" pp_pattern pat pp_term rhs)
      | None -> Fmt.(pf f "@[<missing pattern> ->@;%a@]" pp_term rhs)
    in
    match cases with
    | [] -> ()
    | [ (pat_opt, rhs) ] -> (
        match pat_opt with
        | Some pat ->
            Fmt.(
              pf frmt "@[<hov 2>%a @[%a _x@] %a@;@[match _x with %a -> %a@]@]"
                (styled (`Fg `Cyan) string)
                nt.vname (list ~sep:sp Variable.pp) args
                (styled (`Fg `Red) string)
                "=" pp_pattern pat pp_term rhs)
        | None ->
            Fmt.(
              pf frmt "@[<hov 2>%a @[%a@] %a@;%a@]"
                (styled (`Fg `Cyan) string)
                nt.vname (list ~sep:sp Variable.pp) args
                (styled (`Fg `Red) string)
                "=" pp_term rhs))
    | _ :: _ ->
        Fmt.(
          pf frmt "@[<hov 2>%a @[%a@]%a@;@[<hov 2>%a@;%a@]"
            (styled (`Fg `Cyan) string)
            nt.vname (list ~sep:sp Variable.pp) args
            (styled (`Fg `Red) string)
            "="
            (styled (`Fg `Yellow) string)
            "function"
            (list ~sep:(fun f () -> pf f "@;%a " (styled (`Fg `Yellow) string) "|") pp_case)
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
          let substs = List.map ~f:(fun (x, y) -> (mk_var x, mk_var y)) pre_subst in
          (args, match_cases @ [ (pat, substitution substs rhs) ])
        in
        match Map.to_alist nt_rules with
        | [] -> ([], [])
        | (_, (_, args, pat, rhs)) :: tl ->
            let init = (args, [ (pat, rhs) ]) in
            List.fold ~f:reconstr_cases ~init tl
      in
      (nt, args, match_cases)
    in
    List.map ~f (Set.elements pmrs.pnon_terminals)
  in
  match functions with
  | [] -> ()
  | hd :: tl ->
      Fmt.(pf frmt "@[%a %a@]@." (styled (`Fg `Red) string) "let rec" print_caml_def hd);
      List.iter tl ~f:(fun caml_def -> Fmt.(pf frmt "@[and %a@]@." print_caml_def caml_def))

(* ============================================================================================= *)
(*                             BASIC PROPERTIES AND TYPE INFERENCE                               *)
(* ============================================================================================= *)
(* Update the order of the pmrs. *)
let update_order (p : t) : t =
  let order =
    let f ~key:_ ~data:(_, args, p, _) m =
      max m (List.length args + if Option.is_some p then 1 else 0)
    in
    Map.fold ~f ~init:0 p.prules
  in
  { p with porder = order }

let infer_pmrs_types (prog : t) =
  Log.verbose Fmt.(fun fmt () -> pf fmt "@[<hov 2>Untyped PMRS input:@;@[%a@]@]" pp prog);
  let infer_aux ~key ~data:(nt, args, pat, body) (map, substs) =
    let t_body, c_body = infer_type body in
    let t_head, c_head =
      let head_term =
        let t_args = List.map ~f:mk_var args in
        match pat with
        | Some (pat_cstr, pat_args) -> mk_app (mk_var nt) (t_args @ [ mk_data pat_cstr pat_args ])
        | None -> mk_app (mk_var nt) t_args
      in
      infer_type head_term
    in
    let cur_loc = body.tpos in
    let c_rule = RType.merge_subs cur_loc c_body c_head in
    match RType.unify (RType.mkv (substs @ c_rule) @ [ (t_head.ttyp, t_body.ttyp) ]) with
    | Some res -> (Map.set map ~key ~data:(nt, args, pat, rewrite_types (RType.mkv res) t_body), res)
    | None ->
        Log.loc_fatal_errmsg cur_loc
          (Fmt.str "(%a) has type %a, expected type %a." pp_term body RType.pp t_body.ttyp RType.pp
             t_head.ttyp)
  in
  let new_rules, new_subs = Map.fold prog.prules ~f:infer_aux ~init:(Map.empty (module Int), []) in
  match RType.unify (RType.mkv new_subs) with
  | Some usubs ->
      Variable.update_var_types (RType.mkv usubs);
      let typ_in, typ_out = RType.fun_typ_unpack (Variable.vtype_or_new prog.pmain_symb) in
      Variable.update_var_types
        [ (Variable.vtype_or_new prog.pvar, Variable.vtype_or_new prog.pmain_symb) ];
      let invariant = Option.map ~f:(fun x -> first (infer_type x)) prog.pspec.ensures in
      {
        prog with
        prules = new_rules;
        pinput_typ = typ_in;
        poutput_typ = typ_out;
        pspec = { prog.pspec with ensures = invariant };
      }
  | None -> failwith "Failed infering types for pmrs."

(* ============================================================================================= *)
(*                             TRANSLATION FROM FUNCTION to PMRS                                 *)
(* ============================================================================================= *)

let func_to_pmrs (f : Variable.t) (args : fpattern list) (body : Term.term) =
  let tin, tout =
    match Variable.vtype_or_new f with
    | RType.TFun (tin, tout) -> (tin, tout)
    | _ -> failwith "Cannot make pmrs of non-function."
  in
  let pmain_symb, prules, pnon_terminals =
    match (args, body) with
    | [ PatVar x ], { tkind = TVar x_v; _ } when Variable.(x = x_v) ->
        (f, Map.singleton (module Int) 0 (f, [ x ], None, body), VarSet.singleton x)
    | _ -> failwith "TODO: only identity supported by func_to_pmrs"
  in
  {
    pvar = f;
    pinput_typ = [ tin ];
    poutput_typ = tout;
    pspec = empty_spec;
    pargs = Set.elements (fpat_vars (PatTup args));
    psyntobjs = VarSet.empty;
    (* PMRS from a function cannot have unkowns. *)
    porder = 0;
    pmain_symb;
    prules;
    pnon_terminals;
  }

(* ============================================================================================= *)
(*                                           UTILS FOR PMRS                                      *)
(* ============================================================================================= *)

(**
  inverted_rule_lookup searches for rules whose rhs match (func args), and return
  a map from rule id to the lhs of the rules matching (func args), with the appropriate
  substitutions performed.
*)
let inverted_rule_lookup ?(boundvars = VarSet.empty) rules (func : term) (args : term list) =
  let list_matching l =
    let m = List.map l ~f:(fun (rhs_arg, arg) -> matches ~boundvars ~pattern:rhs_arg arg) in
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
    with _ -> None
  in
  let filter (nt, rule_args, rule_pat, rule_rhs) =
    let lhs_term substs =
      let t = lhs (nt, rule_args, rule_pat, rule_rhs) in
      substitution (Terms.substs_of_alist (Map.to_alist substs)) t
    in
    match rule_rhs.tkind with
    | TApp (rhs_func, rhs_args) ->
        if Terms.(equal rhs_func func) then
          match List.zip rhs_args args with
          | Ok l -> Option.map ~f:lhs_term (list_matching l)
          | _ -> None
        else None
    | _ -> None
  in
  Map.filter_map ~f:filter rules

(** Apply a substitution to all the right hand side of the PMRS rules. *)
let subst_rule_rhs ~(p : t) (substs : (term * term) list) =
  let rules' =
    let f (nt, args, pat, body) =
      let body' = substitution substs body in
      let body'', _ = infer_type body' in
      (nt, args, pat, body'')
    in
    Map.map ~f p.prules
  in
  { p with prules = rules' }
