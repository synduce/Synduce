open Base
open Lang
open Lang.Analysis
open Lang.Term
open Utils

(* ============================================================================================= *)
(*                                     MOST GENERAL TERMS                                        *)
(* ============================================================================================= *)

let mgt (prog : PMRS.t) : TermSet.t =
  let xi = prog.psyntobjs in
  let i = ref 0 in
  let rec aux (mgts, rem_xi) exp_queue =
    Int.incr i;
    if !i > !Config.reduction_limit then None
    else
      let f (mgts, rem_xi, rem_xs) expansion =
        let f_xp = Reduce.reduce_pmrs prog expansion in
        let fv = Analysis.free_variables f_xp in
        let xi_in_xp = Set.inter rem_xi fv in
        if Set.length xi_in_xp > 0 then (Set.add mgts expansion, Set.diff rem_xi xi_in_xp, rem_xs)
        else (mgts, rem_xi, rem_xs @ [ expansion ])
      in
      let g x =
        let xs = expand_once x in
        List.fold ~f ~init:(mgts, rem_xi, []) xs
      in
      match exp_queue with
      | hd :: tl ->
          let mgts, rem_xi, rem_xs = g hd in
          if Set.length rem_xi = 0 then Some mgts else aux (mgts, rem_xi) (tl @ rem_xs)
      | [] -> Some mgts
  in
  let x0 = Variable.mk ~t:(Some !AState._theta) "x0" in
  match aux (TermSet.empty, xi) [ mk_var x0 ] with
  | Some mgts -> mgts
  | None -> TermSet.singleton (mk_var x0)
(* Start the algorith from a variable. *)

let most_general_terms (prog : PMRS.t) : TermSet.t =
  let ts = if Set.is_empty prog.psyntobjs then TermSet.empty else mgt prog in
  ts
