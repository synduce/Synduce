open Alpha
open Automata
open Base
open Term
open Trees
open Utils

open Result.Let_syntax

(* ============================================================================================= *)
(*                      TYPE DEFINITIONS AND UTILS                                               *)
(* ============================================================================================= *)


type pattern = variable * variable list

type rewrite_rule = variable * variable list * pattern option * term

type pmrs = {
  main_id : int;
  rules : (rewrite_rule list) Map.M(Int).t;
  non_terminals : VarSet.t;
  order : int;
}

(* Type shortcuts *)
type 'a xresult = ('a, (string * Sexp.t) list) Result.t
type 'a sresult = ('a, (string * term) list) Result.t
type variables = variable Map.M(String).t

(* ============================================================================================= *)
(*                                 BASIC PROPETIES                                               *)
(* ============================================================================================= *)
(* Update the order of the pmrs. *)
let update_order (p : pmrs) : pmrs =
  let all_rules = List.concat (List.map ~f:snd (Map.to_alist p.rules)) in
  let order =
    let f m (_, args, p, _) =
      max m (List.length args + if Option.is_some p then 1 else 0)
    in
    List.fold ~f ~init:0 all_rules
  in { p with order = order }


(* ============================================================================================= *)
(*                                 PRETTY PRINTING                                               *)
(* ============================================================================================= *)

(* let pp_pattern (frmt : Formatter.t) (t, args : pattern) : unit =
   Fmt.(pf frmt "%s(%a)" t.name (list ~sep:comma string) (Variable.names args))

   let pp_rewrite_rule (frmt : Formatter.t) (nt, vargs, pat, t : rewrite_rule) : unit =
   Fmt.(pf frmt "%s %a %a --> %a"
         nt.name
         (list ~sep:comma string) (Variable.names vargs)
         (option pp_pattern) pat
         (box pp_term) t)

   let pp_pmrs (frmt : Formatter.t) (pmrs : pmrs) : unit =
   List.iter
    ~f:(fun (_, r) ->
        Fmt.(pf frmt "%a@." (list (box pp_rewrite_rule)) r))
    (Map.to_alist pmrs.rules) *)

