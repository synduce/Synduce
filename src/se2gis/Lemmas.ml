open Base
open Common
open Elim
open Lang
open ProblemDefs
open Term

let empty_lemmas () : lemmas = Hashtbl.create (module Terms)

let find_term_info (lemmas : lemmas) ((term, splitter) : term * term option)
    : term_info option
  =
  match Hashtbl.find lemmas term with
  | Some term_infos ->
    List.find ~f:(fun ti -> Option.equal Terms.equal ti.splitter splitter) term_infos
  | None -> None
;;

let add_term_info (lemmas : lemmas) ~(key : term) ~(data : term_info) : unit =
  match Hashtbl.find lemmas key with
  | Some term_infos -> Hashtbl.set lemmas ~key ~data:(data :: term_infos)
  | None -> Hashtbl.add_multi lemmas ~key ~data
;;

let get_lemma ~(ctx : Context.t) ~(p : PsiDef.t) ~(key : term) (ts : lemmas) : term option
  =
  let term_info_to_lemma det =
    (* Recursion elimination substitutions *)
    let subst =
      List.concat_map
        ~f:(fun (t1, t2) ->
          let frt1 = mk_f_compose_r_main ~ctx ~p t1 in
          match t2.tkind with
          | TTup t2s -> List.mapi t2s ~f:(fun i t2_i -> t2_i, mk_sel ctx frt1 i)
          | _ -> [ t2, frt1 ])
        det.recurs_elim
    in
    let f lem = Term.substitution subst lem in
    Option.map
      ~f:(Rewriter.simplify_term ~ctx)
      (mk_assoc Binop.And (List.map ~f det.lemmas))
  in
  match Hashtbl.find_multi ts key with
  | [] -> None
  | tis ->
    (match List.filter_opt (List.map ~f:term_info_to_lemma tis) with
    | [] -> None
    | [ a ] -> Some a
    | _ as conds -> mk_assoc Binop.And conds)
;;

let change_lemma
    (lemmas : lemmas)
    ~(key : term)
    ~(split : term option)
    ~(data : term_info -> term_info)
    : unit
  =
  match Hashtbl.find lemmas key with
  | Some tis ->
    let flag = ref false in
    let tis' =
      List.map
        ~f:(fun ti ->
          if Option.equal Terms.equal ti.splitter split
          then (
            flag := true;
            data ti)
          else ti)
        tis
    in
    if !flag then Hashtbl.set ~key ~data:tis' lemmas
  | None -> ()
;;

let get_precise_lemma
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (ts : lemmas)
    ~(key : term * term option)
    : term option
  =
  let term_detail_to_lemma det =
    let subst =
      List.concat_map
        ~f:(fun (t1, t2) ->
          let frt1 = mk_f_compose_r_main ~ctx ~p t1 in
          match t2.tkind with
          | TTup t2s -> List.mapi t2s ~f:(fun i t2_i -> t2_i, mk_sel ctx frt1 i)
          | _ -> [ t2, frt1 ])
        det.recurs_elim
    in
    let f lem = Term.substitution subst lem in
    Option.map
      ~f:(Rewriter.simplify_term ~ctx)
      (mk_assoc Binop.And (List.map ~f det.lemmas))
  in
  Option.bind ~f:term_detail_to_lemma (find_term_info ts key)
;;
