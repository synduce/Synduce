open Analysis
open Base
open Term
open Utils

(* ============================================================================================= *)
(*                      TYPE DEFINITIONS AND UTILS                                               *)
(* ============================================================================================= *)


type pattern = string * term list

type rewrite_rule = variable * variable list * pattern option * term

type top_function = variable * variable list * term

type t = {
  pname : string;
  pinput_typ : RType.t;
  poutput_typ : RType.t * term option;
  pargs : variable list;
  pparams : VarSet.t;
  prules : rewrite_rule IntMap.t;
  pnon_terminals : VarSet.t;
  pmain_symb : variable;
  porder : int;
}

(* Type shortcuts *)
type 'a xresult = ('a, (string * Sexp.t) list) Result.t
type 'a sresult = ('a, (string * term) list) Result.t
type variables = variable Map.M(String).t

let lhs (nt, args, pat, rhs) =
  let all_args =
    let args = List.map ~f:mk_var args in
    match pat with
    | Some (c, pat_args) -> args @ [mk_data c pat_args]
    | None -> args
  in
  let t, _ = infer_type (mk_app ~pos:rhs.tpos (mk_var nt) all_args) in
  t

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
  in { p with porder = order }


let infer_pmrs_types (prog : t) =
  let infer_aux ~key ~data:(nt, args, pat, body) (map, substs) =
    let t_body, c_body = infer_type body in
    let t_head, c_head =
      let head_term =
        let t_args = List.map ~f:mk_var args in
        match pat with
        | Some (pat_cstr, pat_args) -> mk_app (mk_var nt) (t_args @ [mk_data pat_cstr pat_args])
        | None -> mk_app (mk_var nt) t_args
      in
      infer_type head_term
    in
    let cur_loc = body.tpos in
    let c_rule = RType.merge_subs cur_loc c_body c_head in
    match RType.unify ((RType.mkv (substs @ c_rule)) @ [t_head.ttyp, t_body.ttyp]) with
    | Some res -> Map.set map ~key ~data:(nt, args, pat, rewrite_types (RType.mkv res) t_body), res
    | None -> Log.loc_fatal_errmsg cur_loc
                (Fmt.str "(%a) has type %a, expected type %a."
                   pp_term body RType.pp t_body.ttyp RType.pp t_head.ttyp)
  in
  let new_rules, new_subs =
    Map.fold prog.prules ~f:infer_aux ~init:(Map.empty (module Int), [])
  in
  match RType.unify (RType.mkv new_subs) with
  | Some usubs ->
    Variable.update_var_types (RType.mkv usubs);
    let in_typ =
      match Variable.vtype_or_new prog.pmain_symb with
      | RType.TFun (TTup [tin],_) -> tin
      | _ -> failwith "PMRS with multiple inputs not supported."
    in
    { prog with prules = new_rules; pinput_typ = in_typ;}
  | None -> failwith "Failed infering types for pmrs."


(* ============================================================================================= *)
(*                                 PRETTY PRINTING                                               *)
(* ============================================================================================= *)
let pp_pattern (frmt : Formatter.t) (t, args : pattern) : unit =
  if List.length args = 0 then
    Fmt.(pf frmt "%s" t)
  else
    Fmt.(pf frmt "%s(%a)" t (list ~sep:comma Term.pp_term) args)


let pp_rewrite_rule (frmt : Formatter.t) (nt, vargs, pat, t : rewrite_rule) : unit =
  Fmt.(pf frmt "@[<hov 2>%s %a %a  ⟹  %a@]"
         nt.vname
         (list ~sep:comma Variable.pp) vargs
         (option pp_pattern) pat
         (box pp_term) t)


let pp (frmt : Formatter.t) (pmrs : t) : unit =
  let pp_rules frmt () =
    Map.iteri
      ~f:(fun ~key:_ ~data:(nt,args,pat,res) ->
          if Variable.(pmrs.pmain_symb = nt) then
            Fmt.(pf frmt "@[<v 2>‣ %a@]@;" pp_rewrite_rule (nt,args,pat,res))
          else
            Fmt.(pf frmt "@[<v 2>  %a@]@;" pp_rewrite_rule (nt,args,pat,res))
        ) pmrs.prules
  in
  Fmt.(pf frmt "%s⟨%a⟩: %a = @;@[<v 2>{@;%a@;}@]"
         pmrs.pname
         VarSet.pp_var_names pmrs.pparams
         RType.pp (Variable.vtype_or_new pmrs.pmain_symb)
         pp_rules ())


(* ============================================================================================= *)
(*                                       REDUCTION                                               *)
(* ============================================================================================= *)

(** Looks for a set of applicable rules in prules to rewrite (f fargs) and return
    the result of applying the possible rules.
    If there is no rule that is applicable, then return an empty list.
*)
let rule_lookup prules (f : variable) (fargs : term list) : term list =
  let app_sub bindv bindto expr =
    let bindt = List.map ~f:mk_var bindv in
    match List.map2 ~f:Utils.pair bindt bindto with
    | Ok x -> Some (substitution x expr)
    | _ -> None
  in
  let f (nt, rule_args, rule_pat, rhs) =
    if Variable.(nt = f) then
      match rule_pat with
      (* We have a pattern, try to match it. *)
      | Some (cstr, pat_args) ->
        (match List.last fargs, List.drop_last fargs with
         | Some pat_match, Some first_args ->
           (match matches pat_match ~pattern:(mk_data cstr pat_args) with
            | Some  bindto_map  ->
              let bindto_list = Map.to_alist bindto_map in
              let pat_v, pat_bto = List.unzip bindto_list in
              app_sub (rule_args @ pat_v) (first_args @ pat_bto) rhs
            | None -> None)
         | _ -> None)
      (* Pattern is empty. Simple substitution. *)
      | None -> app_sub rule_args fargs rhs
    else
      None
  in
  second (List.unzip (Map.to_alist (Map.filter_map prules ~f)))

let reduce (prog : t) (input : term) =
  let one_step t0 =
    let rstep = ref false in
    let rewrite_rule _t =
      match _t.tkind with
      | TApp({tkind=(TVar(f)); _}, fargs) ->
        (match rule_lookup prog.prules f fargs with
         | [] -> _t
         | hd :: _ -> rstep := true; hd)
      | _ -> _t
    in
    let t0' = reduce_term (rewrite_with rewrite_rule t0) in
    t0', !rstep
  in
  let f_input = mk_app (mk_var prog.pmain_symb) [input] in
  let steps = ref 0 in
  let rec apply_until_irreducible t =
    Int.incr steps;
    let t', reduced =  one_step t in
    if reduced then apply_until_irreducible t' else t'
  in
  let res = apply_until_irreducible f_input in
  res

(**
   inverted_rule_lookup searches for rules whose rhs match (func args), and return
   a map from rule id to the lhs of the rules matching (func args), with the appropriate
   substitutions performed.
*)
let inverted_rule_lookup rules (func : term) (args : term list) =
  let list_matching l =
    let m = List.map l ~f:(fun (rhs_arg, arg) -> matches ~pattern:rhs_arg arg) in
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
    | TApp(rhs_func, rhs_args) ->
      if Terms.(equal rhs_func func) then
        (match List.zip rhs_args args with
         | Ok l -> Option.map ~f:lhs_term (list_matching l)
         | _ -> None)
      else None
    | _ -> None
  in
  Map.filter_map ~f:filter rules


(* ============================================================================================= *)
(*                             TRANSLATION FROM FUNCTION to PMRS                                 *)
(* ============================================================================================= *)

let func_to_pmrs (f : Variable.t) (args : fpattern list) (body : Term.term) =
  let tin, tout = match Variable.vtype_or_new f with
    | RType.TFun (tin, tout) -> tin, tout
    | _ -> failwith "Cannot make pmrs of non-function."
  in
  let pmain_symb, prules, pnon_terminals =
    match args, body with
    | [PatVar x], {tkind = TVar(x_v); _} when Variable.(x = x_v) ->
      f, Map.singleton (module Int) 0 (f, [x], None, body), VarSet.singleton x
    | _ -> failwith "TODO: only identity supported by func_to_pmrs"
  in
  {
    pname = f.vname;
    pinput_typ = tin;
    poutput_typ = tout, None;
    pargs = Set.elements (fpat_vars (PatTup args));
    pparams = VarSet.empty; (* PMRS from a function cannot have unkowns. *)
    porder = 0;
    pmain_symb = pmain_symb;
    prules = prules;
    pnon_terminals = pnon_terminals;
  }


let is_identity (p : t) =
  let input_symb = Variable.mk ~t:(Some p.pinput_typ) "e" in
  match reduce p (mk_var input_symb) with
  | {tkind = TVar x; _} -> Variable.(x = input_symb)
  | _ -> false


let subst_rule_rhs ~(p : t) (substs : (term * term) list) =
  let rules' =
    let f (nt, args, pat, body) =
      let body' = substitution substs body in
      let body'', _ = infer_type body' in
      nt, args, pat, body''
    in
    Map.map ~f p.prules
  in
  {p with prules = rules'}


let instantiate_with_solution (p : t) (soln : (string * variable list * term) list) =
  let xi_set = p.pparams in
  let xi_substs =
    let f (name, args, body) =
      match VarSet.find_by_name xi_set name with
      | Some xi -> [Term.mk_var xi, mk_fun (List.map ~f:(fun x -> PatVar x) args) body]
      | None -> []
    in List.concat (List.map ~f soln)
  in
  let target_inst = subst_rule_rhs xi_substs ~p in
  target_inst