(** All functions in this module have no effect on the flow of the algorithm.
  Their only purpose is to log or print information about the current state of
  the algorithm.
*)

open AState
open Base
open Fmt
open Lang
open Term
open Utils

let show_stat elapsed tsize usize =
  if !Config.timings
  then
    pf
      stdout
      "%i,%3.3f,%3.3f,%i,%i@."
      !refinement_steps
      !Stats.verif_time
      elapsed
      tsize
      usize
  else if !Config.json_progressive && !Config.json_out
  then (
    let json = `Assoc [ "progress", LogJson.refinement_steps_summary () ] in
    pf stdout "%s@." (Yojson.to_string ~std:false json))
  else ()
;;

let show_steps tsize usize =
  Log.info (fun frmt () ->
      (styled
         (`Fg `Black)
         (styled
            (`Bg (`Hi `Green))
            (fun frmt (i, j) -> pf frmt "\t\t Refinement step %i:%i " i j)))
        frmt
        (!refinement_steps, !secondary_refinement_steps));
  Log.debug_msg
    (str "Start refinement loop with %i terms in T, %i terms in U." tsize usize)
;;

let show_counterexamples lstate t_set =
  Log.debug (fun frmt () ->
      pf
        frmt
        "@[<hov 2>Counterexample terms:@;@[<hov 2>%a@]"
        (list ~sep:comma Term.pp_term)
        (Set.elements (Set.diff t_set lstate.t_set)))
;;

let show_summary (spec_fname, repr_fname, target_fname) target_f =
  Log.info (fun fmt () ->
      pf
        fmt
        " Ψ (%a) := ∀ x : %a. (%s o %s)(x) = %s(x)"
        (list ~sep:comma Term.Variable.pp)
        (Set.elements target_f.PMRS.psyntobjs)
        (list ~sep:sp RType.pp)
        target_f.pinput_typ
        spec_fname
        repr_fname
        target_fname)
;;

let show_pmrs pmrs =
  Log.info (fun fmt () -> pf fmt "%a" (box (PMRS.pp ~short:(not !Config.verbose))) pmrs)
;;

let msg_too_many_opts () =
  Log.info (fun fmt () ->
      pf
        fmt
        "@[It seems some optimizations caused an error.@;\
         Turning them off and trying again.@]")
;;

(* ============================================================================================= *)
(*                           Messages from the Lemma Synthesis                                   *)
(* ============================================================================================= *)

let no_spurious_ctex () =
  Log.info (fun fmt () ->
      pf fmt "All counterexamples are non-spurious: nothing to refine.")
;;

let spurious_violates_requires i =
  Log.info (fun fmt () -> pf fmt "%i counterexamples violate requires." i)
;;

let print_infeasible_message t_set =
  Log.info (fun frmt () ->
      pf
        frmt
        "@[<hov 2>This problem has no solution. Counterexample set:@;%a@]"
        (list ~sep:sp Term.pp_term)
        (Set.elements t_set))
;;

let announce_new_lemma_synthesis (thread_no : int) (det : term_state_detail) =
  Log.debug (fun f () ->
      match det.current_preconds with
      | None ->
        pf
          f
          "[%i] Synthesizing a new lemma candidate for term@;@[%a[%a]@]."
          thread_no
          pp_term
          det.term
          pp_subs
          det.recurs_elim
      | Some pre ->
        pf
          f
          "[%i] Synthesizing a new lemma candidate for term@;\
           @[%a[%a]@]@;\
           with precondition@;\
           @[%a@]"
          thread_no
          pp_term
          det.term
          pp_subs
          det.recurs_elim
          pp_term
          pre)
;;

let lemma_not_proved_correct () =
  Log.verbose (fun f () ->
      pf f "This lemma has not been proven correct. Refining lemma...")
;;

let lemma_proved_correct (det : term_state_detail) (lemma_term : term) =
  let lemma_term = Eval.simplify lemma_term in
  Log.verbose (fun f () -> pf f "This lemma has been proven correct.");
  let lemma_str =
    str
      "(%a)[%a]->%s(%s)= %a"
      pp_term
      det.term
      (list ~sep:comma (Fmt.pair ~sep:rightarrow pp_term pp_term))
      det.recurs_elim
      det.lemma.vname
      (String.concat ~sep:", " (List.map ~f:(fun v -> v.vname) det.scalar_vars))
      (box pp_term)
      lemma_term
  in
  Stats.set_lemma_synthesized
    "requires_lemma"
    (String.concat ~sep:" " (List.map ~f:String.strip (String.split_lines lemma_str)));
  Log.info (fun frmt () ->
      pf
        frmt
        "Lemma for term %a:@;\"%s %s =@;@[%a@]\"."
        pp_term
        det.term
        det.lemma.vname
        (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) det.scalar_vars))
        (box pp_term)
        lemma_term)
;;

(* Messages from ImagePredicates *)

let violates_ensures p ctexs =
  List.iter ctexs ~f:(fun ctex ->
      List.iter ctex.ctex_eqn.eelim ~f:(fun (_, elimv) ->
          let tval = Eval.in_model ctex.ctex_model elimv in
          Log.verbose (fun fmt () ->
              pf
                fmt
                "%a should not be in the image of %s"
                pp_term
                tval
                p.psi_reference.pvar.vname)))
;;

let positives_ensures p positives =
  Log.verbose (fun fmt () ->
      pf
        fmt
        "@[These examples are in the image of %s:@;%a@]"
        p.psi_reference.pvar.vname
        (list ~sep:comma pp_term)
        positives)
;;

let log_new_ensures (new_pred : term) : unit =
  Stats.set_lemma_synthesized "ensures_lemma" (String.strip (str "%a" pp_term new_pred))
;;
