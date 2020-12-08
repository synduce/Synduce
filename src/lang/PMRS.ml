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
  pargs : VarSet.t;
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

(* ============================================================================================= *)
(*                             BASIC PROPETIES AND TYPE INFERENCE                                *)
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
    | None -> Log.loc_fatal_errmsg cur_loc "Unification failed between head and body."
  in
  let new_rules, new_subs =
    Map.fold prog.prules ~f:infer_aux ~init:(Map.empty (module Int), [])
  in
  match RType.unify (RType.mkv new_subs) with
  | Some usubs ->
    Variable.update_var_types (RType.mkv usubs);
    let in_typ =
      match Variable.vtype_or_new prog.pmain_symb with
      | RType.TFun (tin,_) -> tin
      | t -> t
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
         RType.pp pmrs.pinput_typ
         pp_rules ())


(* ============================================================================================= *)
(*                                       REDUCTION                                               *)
(* ============================================================================================= *)

let reduce (prog : t) (input : term) =
  let rule_m f fargs =
    let app_sub bindv bindto expr =
      let bindt = List.map ~f:mk_var bindv in
      match List.map2 ~f:Utils.pair bindt bindto with
      | Ok x -> Some (substitution x expr)
      | _ -> None
    in
    let f (nt, args, pat, res) =
      if Variable.(nt = f) then
        match pat with
        (* We have a pattern, try to match it. *)
        | Some (cstr, pat_args) ->
          (match List.last fargs, List.drop_last fargs with
           | Some pat_match, Some first_args ->
             (match matches pat_match ~pattern:(mk_data cstr pat_args) with
              | Some  bindto_map  ->
                let bindto_list = Map.to_alist bindto_map in
                let pat_v, pat_bto = List.unzip bindto_list in
                app_sub (args @ pat_v) (first_args @ pat_bto) res
              | None -> None)
           | _ -> None)
        (* Pattern is empty. Simple substitution. *)
        | None -> app_sub args fargs res
      else
        None
    in
    let _, b = List.unzip (Map.to_alist (Map.filter_map prog.prules ~f)) in
    b
  in
  let one_step t0 =
    let rstep = ref false in
    let rewrite_rule _t =
      match _t.tkind with
      | TApp({tkind=(TVar(f)); _}, fargs) ->
        (match rule_m f fargs with
         | [] -> _t
         | hd :: _ -> rstep := true; hd)
      | _ -> _t
    in
    let t0' = rewrite_with rewrite_rule t0 in
    t0', !rstep
  in
  let f_input = mk_app (mk_var prog.pmain_symb) [input] in
  let steps = ref 0 in
  let rec apply_until_irreducible t =
    Int.incr steps;
    let t', reduced =  one_step t in
    if reduced && !steps <  !Config.reduction_limit then apply_until_irreducible t' else t'
  in
  let res = apply_until_irreducible f_input in
  res


(* ============================================================================================= *)
(*                             TRANSLATION FROM FUNCTION to PMRS                                 *)
(* ============================================================================================= *)

let func_to_pmrs (f : Variable.t) (args : Variable.t list) (body : Term.term) =
  let tin, _ = match Variable.vtype_or_new f with
    | RType.TFun (tin, tout) -> tin, tout
    | _ -> failwith "Cannot make pmrs of non-function."
  in
  let pmain_symb, prules, pnon_terminals =
    match args, body with
    | [x], {tkind = TVar(x_v); _} when Variable.(x = x_v) ->
      f, Map.singleton (module Int) 0 (f, [x], None, body), VarSet.singleton x
    | _ -> failwith "TODO: only identity supported by func_to_pmrs"
  in
  {
    pname = f.vname;
    pinput_typ = tin;
    pargs = VarSet.of_list args ;
    pparams = VarSet.empty; (* PMRS from a function cannot have unkowns. *)
    porder = 0;
    pmain_symb = pmain_symb;
    prules = prules;
    pnon_terminals = pnon_terminals;
  }