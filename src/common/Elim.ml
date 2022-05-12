open Base
open Env
open Lang
open Term
open ProblemDefs

(** Given a term and a problem definition, output a recursion elimination map (a list associating some
 terms of recursive type to terms of bounded type). The second element of the pair returned by this
 function is the set of scalar variables that would appear in the term after recursion elimination has
 been applied.
*)
let recurs_elim_of_term ~(ctx : env) (term : term) ~(p : PsiDef.t)
    : (term * term) list * variable list
  =
  Set.fold
    ~init:([], p.PsiDef.reference.pargs)
    ~f:(fun (rec_elim, vars) var ->
      match Variable.vtype ctx.ctx var with
      | None -> rec_elim, var :: vars
      | Some t ->
        if RType.is_recursive ctx.ctx.types t
        then (
          match Expand.mk_recursion_elimination_term ~ctx p with
          | None -> rec_elim, vars
          | Some (a, _) ->
            (* When are a and b different? *)
            ( (mk_var ctx.ctx var, a) :: rec_elim
            , vars @ Set.elements (ctx >- Analysis.free_variables a) ))
        else rec_elim, var :: vars)
    (ctx >- Analysis.free_variables term)
;;

let flatten_rec_elim_tuples ~(ctx : Context.t) (elim : (term * term) list)
    : (term * term) list
  =
  List.concat_map
    ~f:(fun (a, b) ->
      match b.tkind with
      | TTup comps -> List.mapi comps ~f:(fun i t -> mk_sel ctx a i, t)
      | _ -> [ a, b ])
    elim
;;

(** Construct a substitution from images in elim2 to images in elim1. *)
let subs_from_elim_to_elim ~ctx elim1 elim2 : (term * term) list =
  List.concat_map
    ~f:(fun (r1, s1) ->
      let rec f lst =
        match lst with
        | [] -> []
        | (r2, s2) :: tl -> if Terms.(equal r1 r2) then [ s2, s1 ] else f tl
      in
      f (flatten_rec_elim_tuples ~ctx elim2))
    (flatten_rec_elim_tuples ~ctx elim1)
;;

let mk_f_compose_r_orig ~(ctx : Context.t) ~(p : PsiDef.t) (t : term) : term =
  let repr_of_v =
    if p.PsiDef.repr_is_identity then t else mk_app_v ctx p.PsiDef.repr.pvar [ t ]
  in
  mk_app_v ctx p.PsiDef.reference.pvar [ repr_of_v ]
;;

let mk_f_compose_r_main ~(ctx : Context.t) ~(p : PsiDef.t) (t : term) : term =
  let repr_of_v =
    if p.PsiDef.repr_is_identity then t else mk_app_v ctx p.PsiDef.repr.pmain_symb [ t ]
  in
  mk_app_v ctx p.PsiDef.reference.pmain_symb [ repr_of_v ]
;;
