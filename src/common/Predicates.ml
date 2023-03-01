open Base
open Env
open Elim
open Lang
open ProblemDefs
open Term
open Utils

let cond_lemma_to_term
    ~(ctx : env)
    ~(p : PsiDef.t)
    ~(t : term)
    (ti : term_info)
    (cl : cond_lemma)
  =
  (* Term adjustment substituions *)
  match ctx >- Matching.matches ~pattern:ti.ti_term t with
  | Some subst1 ->
    (* Recursion elimination substitutions *)
    let subst =
      List.concat_map
        ~f:(fun (t1, t2) ->
          let frt1 = ctx >- mk_f_compose_r_main ~p t1 in
          match t2.tkind with
          | TTup t2s -> List.mapi t2s ~f:(fun i t2_i -> t2_i, mk_sel ctx.ctx frt1 i)
          | _ -> [ t2, frt1 ])
        ti.ti_elim
    in
    let apply_subs lem =
      let t1 = Term.substitution subst lem in
      Term.substitution (VarMap.to_subst ctx.ctx subst1) t1
    in
    let lems = List.map ~f:apply_subs cl.cl_lemmas in
    Option.(mk_assoc Binop.And lems >>| (ctx >- Rewriter.simplify_term))
  | None -> None
;;

let key_of_term (t : term) = Expression.(Option.map ~f:nameless_normal_form (of_term t))

let get ?(count_reuse = true) ~(ctx : env) ~(p : PsiDef.t) (t : term) =
  match key_of_term t with
  | None -> None
  | Some e_key ->
    (match Hashtbl.find ctx.pcache e_key with
    | None -> None
    | Some (ti, cls) ->
      let same_psi = ti.ti_psi_id = p.id in
      if (not same_psi) && not !Config.Optims.reuse_predicates
      then None
      else (
        if same_psi || not count_reuse
        then ()
        else Int.incr Utils.Stats.num_foreign_lemma_uses;
        match List.filter_opt (List.map ~f:(cond_lemma_to_term ~ctx ~p ~t ti) cls) with
        | [] -> None
        | [ a ] -> Some a
        | _ as conds -> mk_assoc Binop.And conds))
;;

let find_lemma_info ~(ctx : Env.env) ((term, splitter) : term * term option)
    : (term_info * cond_lemma option) option
  =
  match Option.bind ~f:(Hashtbl.find ctx.pcache) (key_of_term term) with
  | Some (ti, cls) ->
    Some (ti, List.find ~f:(fun cl -> Option.equal Terms.equal cl.cl_cond splitter) cls)
  | None -> None
;;

let find ~(ctx : env) ~(key : term) =
  Option.bind ~f:(fun e_key -> Hashtbl.find ctx.pcache e_key) (key_of_term key)
;;

let get_with_precond ~(ctx : env) ~(p : PsiDef.t) ~(key : term * term option) =
  match find_lemma_info ~ctx key with
  | Some (ti, Some cl) -> cond_lemma_to_term ~ctx ~p ~t:(Utils.first key) ti cl
  | Some (_, None) -> None
  | _ -> None
;;

let change
    ~(ctx : env)
    ~(key : term)
    ~(split : term option)
    (data_f : term_info -> cond_lemma -> cond_lemma Lwt.t)
    : unit Lwt.t
  =
  match key_of_term key with
  | Some e_key ->
    (match Hashtbl.find ctx.pcache e_key with
    | Some (ti, cls) ->
      let flag = ref false in
      let%lwt cls' =
        Lwt_list.map_p
          (fun cl ->
            if Option.equal Terms.equal cl.cl_cond split
            then (
              flag := true;
              data_f ti cl)
            else Lwt.return cl)
          cls
      in
      Lwt.return (if !flag then Hashtbl.set ~key:e_key ~data:(ti, cls') ctx.pcache)
    | None -> Lwt.return ())
  | None -> Lwt.return ()
;;

let add_direct
    ~(ctx : env)
    ~(key : Expression.t)
    ~data:((ti, newcl) : term_info * cond_lemma)
    : unit
  =
  match Hashtbl.find ctx.pcache key with
  | Some (term_info, cls) ->
    let a, b =
      List.partition_tf
        ~f:(fun cl -> Option.equal Terms.equal cl.cl_cond newcl.cl_cond)
        cls
    in
    (match a with
    | [] -> Hashtbl.set ctx.pcache ~key ~data:(term_info, newcl :: cls)
    | [ _ ] -> Hashtbl.set ctx.pcache ~key ~data:(term_info, newcl :: b)
    | _ -> failwith "Should not be more than one equivalent key")
  | None -> Hashtbl.set ctx.pcache ~key ~data:(ti, [ newcl ])
;;

let add ~(ctx : env) ~(key : term) ~(data : term_info * cond_lemma) : unit =
  match key_of_term key with
  | Some e_key -> add_direct ~ctx ~key:e_key ~data
  | None -> failwith "Failed to add predicate"
;;

let set ~(ctx : env) ~(key : term) ~(data : term_info * cond_lemma list) : unit =
  match key_of_term key with
  | Some e_key -> Hashtbl.set ctx.pcache ~key:e_key ~data
  | None -> failwith "Failed to set predicate"
;;

let fold
    ~(ctx : env)
    ~(init : 'a)
    ~(f : key:Expression.t -> data:term_info * cond_lemma list -> 'a -> 'a)
  =
  Hashtbl.fold ctx.pcache ~init ~f
;;
