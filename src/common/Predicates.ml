open Base
open Env
open Elim
open Lang
open ProblemDefs
open Term

let term_info_to_lemma ~(ctx : env) ~(p : PsiDef.t) ~(t : term) (det : term_info) =
  (* Term adjustment substituions *)
  let subst1 = ctx >- Matching.matches ~pattern:det.ti_term t in
  (* Recursion elimination substitutions *)
  let subst =
    List.concat_map
      ~f:(fun (t1, t2) ->
        let frt1 = ctx >- mk_f_compose_r_main ~p t1 in
        match t2.tkind with
        | TTup t2s -> List.mapi t2s ~f:(fun i t2_i -> t2_i, mk_sel ctx.ctx frt1 i)
        | _ -> [ t2, frt1 ])
      det.ti_elim
  in
  let f lem =
    let t1 = Term.substitution subst lem in
    match subst1 with
    | None -> None
    | Some subst' -> Some (Term.substitution (VarMap.to_subst ctx.ctx subst') t1)
  in
  let lems = Option.all (List.map ~f det.ti_lemmas) in
  Option.(lems >>= mk_assoc Binop.And >>| (ctx >- Rewriter.simplify_term))
;;

let key_of_term (t : term) = Expression.(Option.map ~f:nameless_normal_form (of_term t))

let get ~(ctx : env) ~(p : PsiDef.t) (t : term) =
  match key_of_term t with
  | None -> None
  | Some e_key ->
    (match Hashtbl.find_multi ctx.pcache e_key with
    | [] -> None
    | tis ->
      (match List.filter_opt (List.map ~f:(term_info_to_lemma ~ctx ~p ~t) tis) with
      | [] -> None
      | [ a ] -> Some a
      | _ as conds -> mk_assoc Binop.And conds))
;;

let find_term_info ~(ctx : Env.env) ((term, splitter) : term * term option)
    : term_info option
  =
  match Option.bind ~f:(Hashtbl.find ctx.pcache) (key_of_term term) with
  | Some term_infos ->
    List.find ~f:(fun ti -> Option.equal Terms.equal ti.ti_splitter splitter) term_infos
  | None -> None
;;

let find ~(ctx : env) ~(key : term) =
  Option.bind ~f:(fun e_key -> Hashtbl.find ctx.pcache e_key) (key_of_term key)
;;

let get_with_precond ~(ctx : env) ~(p : PsiDef.t) ~(key : term * term option)
    : term option
  =
  Option.bind
    ~f:(term_info_to_lemma ~ctx ~p ~t:(Utils.first key))
    (find_term_info ~ctx key)
;;

let change
    ~(ctx : env)
    ~(key : term)
    ~(split : term option)
    (data : term_info -> term_info)
    : unit
  =
  match key_of_term key with
  | Some e_key ->
    (match Hashtbl.find ctx.pcache e_key with
    | Some tis ->
      let flag = ref false in
      let tis' =
        List.map
          ~f:(fun ti ->
            if Option.equal Terms.equal ti.ti_splitter split
            then (
              flag := true;
              data ti)
            else ti)
          tis
      in
      if !flag then Hashtbl.set ~key:e_key ~data:tis' ctx.pcache
    | None -> ())
  | None -> ()
;;

let add_direct ~(ctx : env) ~(key : Expression.t) ~(data : term_info) : unit =
  match Hashtbl.find ctx.pcache key with
  | Some term_infos ->
    let a, b =
      List.partition_tf
        ~f:(fun det -> Option.equal Terms.equal det.ti_splitter data.ti_splitter)
        term_infos
    in
    (match a with
    | [] -> Hashtbl.set ctx.pcache ~key ~data:(data :: term_infos)
    | [ _ ] -> Hashtbl.set ctx.pcache ~key ~data:(data :: b)
    | _ -> failwith "Should not be more than one equivalent key")
  | None -> Hashtbl.add_multi ctx.pcache ~key ~data
;;

let add ~(ctx : env) ~(key : term) ~(data : term_info) : unit =
  match key_of_term key with
  | Some e_key -> add_direct ~ctx ~key:e_key ~data
  | None -> failwith "Failed to add predicate"
;;

let set ~(ctx : env) ~(key : term) ~(data : term_info list) : unit =
  match key_of_term key with
  | Some e_key -> Hashtbl.set ctx.pcache ~key:e_key ~data
  | None -> failwith "Failed to set predicate"
;;

let fold
    ~(ctx : env)
    ~(init : 'a)
    ~(f : key:Expression.t -> data:term_info list -> 'a -> 'a)
  =
  Hashtbl.fold ctx.pcache ~init ~f
;;
