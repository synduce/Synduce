open Base
open Term
open Utils

(* ============================================================================================= *)
(*                      TYPE DEFINITIONS AND UTILS                                               *)
(* ============================================================================================= *)


type pattern = string * variable list

type rewrite_rule = variable * variable list * pattern option * term

type pmrs = {
  pname : string;
  pargs : VarSet.t;
  pparams : VarSet.t;
  pmain_id : int;
  prules : rewrite_rule IntMap.t;
  pnon_terminals : VarSet.t;
  porder : int;
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
  let order =
    let f ~key:_ ~data:(_, args, p, _) m =
      max m (List.length args + if Option.is_some p then 1 else 0)
    in
    Map.fold ~f ~init:0 p.prules
  in { p with porder = order }


(* ============================================================================================= *)
(*                                 PRETTY PRINTING                                               *)
(* ============================================================================================= *)

let pp_pattern (frmt : Formatter.t) (t, args : pattern) : unit =
  if List.length args = 0 then
    Fmt.(pf frmt "%s" t)
  else
    Fmt.(pf frmt "%s(%a)" t (list ~sep:comma Variable.pp) args)

let pp_rewrite_rule (frmt : Formatter.t) (nt, vargs, pat, t : rewrite_rule) : unit =
  Fmt.(pf frmt "@[<hov 2>%s %a %a  ⟹  %a@]"
         nt.vname
         (list ~sep:comma Variable.pp) vargs
         (option pp_pattern) pat
         (box pp_term) t)

let pp_pmrs (frmt : Formatter.t) (pmrs : pmrs) : unit =
  let pp_rules frmt () =
    Map.iteri
      ~f:(fun ~key:i ~data:rrule ->
          if i = pmrs.pmain_id then
            Fmt.(pf frmt "@[<v 2>‣ %a@]@;" pp_rewrite_rule rrule)
          else
            Fmt.(pf frmt "@[<v 2>  %a@]@;" pp_rewrite_rule rrule)
        ) pmrs.prules
  in
  Fmt.(pf frmt "%s[%a]:@;@[<v 2>{@;%a@;}@]"
         pmrs.pname
         VarSet.pp_var_names pmrs.pparams
         pp_rules ())
