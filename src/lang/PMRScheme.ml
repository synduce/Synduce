open Analysis
open Base
open Term
open Utils

(* ============================================================================================= *)
(*                      TYPE DEFINITIONS AND UTILS                                               *)
(* ============================================================================================= *)


type pattern = string * term list

type rewrite_rule = variable * variable list * pattern option * term

type pmrs = {
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
let update_order (p : pmrs) : pmrs =
  let order =
    let f ~key:_ ~data:(_, args, p, _) m =
      max m (List.length args + if Option.is_some p then 1 else 0)
    in
    Map.fold ~f ~init:0 p.prules
  in { p with porder = order }

let infer_pmrs_types (prog : pmrs) =
  let infer_aux ~key ~data:(nt, args, pat, body) (map, substs) =
    Fmt.(pf stdout "Nont: %a@." RType.pp (Variable.vtype_or_new nt));
    Fmt.(pf stdout "Substs: %a@." (list ~sep:sp (parens (pair ~sep:comma int RType.pp))) substs);
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
    Fmt.(pf stdout "%a => %a@."
           RType.pp t_head.ttyp RType.pp t_body.ttyp);
    let c_rule = RType.merge_subs cur_loc c_body c_head in
    match RType.unify ((RType.mkv (substs @ c_rule))) with
    | Some res -> Map.set map ~key ~data:(nt, args, pat, t_body), res
    | None -> Log.loc_fatal_errmsg cur_loc "Unification failed between head and body."
  in
  let new_rules, new_subs =
    Map.fold prog.prules ~f:infer_aux ~init:(Map.empty (module Int), [])
  in
  match RType.unify (RType.mkv new_subs) with
  | Some usubs ->
    Variable.update_var_types (RType.mkv usubs);
    {
      prog with prules = new_rules;
    }
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

let pp_pmrs (frmt : Formatter.t) (pmrs : pmrs) : unit =
  let pp_rules frmt () =
    Map.iteri
      ~f:(fun ~key:_ ~data:(nt,args,pat,res) ->
          if Variable.(pmrs.pmain_symb = nt) then
            Fmt.(pf frmt "@[<v 2>‣ %a@]@;" pp_rewrite_rule (nt,args,pat,res))
          else
            Fmt.(pf frmt "@[<v 2>  %a@]@;" pp_rewrite_rule (nt,args,pat,res))
        ) pmrs.prules
  in
  Fmt.(pf frmt "%s⟨%a⟩:@;@[<v 2>{@;%a@;}@]"
         pmrs.pname
         VarSet.pp_var_names pmrs.pparams
         pp_rules ())


(* ============================================================================================= *)
(*                                       REDUCTION                                               *)
(* ============================================================================================= *)

let reduce (prog : pmrs) (input : term) =
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
    rewrite_with rewrite_rule t0, !rstep
  in
  let f_input = mk_app (mk_var prog.pmain_symb) [input] in
  let steps = ref 0 in
  let rec apply_until_irreducible t =
    Int.incr steps;
    let t', reduced =  one_step t in
    if reduced && !steps <  !Config.reduction_limit then apply_until_irreducible t' else t'
  in apply_until_irreducible f_input
