open AState
open Base
open Lang
open Term
open Utils

let show_stat elapsed tsize usize =
  if !Config.timings
  then
    Fmt.(
      pf
        stdout
        "%i,%3.3f,%3.3f,%i,%i@."
        !refinement_steps
        !Stats.verif_time
        elapsed
        tsize
        usize)
  else if !Config.json_progressive && !Config.json_out
  then (
    let json = `Assoc [ "progress", LogJson.refinement_steps_summary () ] in
    Fmt.(pf stdout "%s@." (Yojson.to_string ~std:false json)))
  else ()
;;

let show_steps tsize usize =
  Log.info
    Fmt.(
      fun frmt () ->
        (styled
           (`Fg `Black)
           (styled
              (`Bg (`Hi `Green))
              (fun frmt (i, j) -> pf frmt "\t\t Refinement step %i - %i " i j)))
          frmt
          (!refinement_steps, !secondary_refinement_steps));
  Log.debug_msg
    Fmt.(str "Start refinement loop with %i terms in T, %i terms in U." tsize usize)
;;

let show_counterexamples lstate t_set =
  Log.debug (fun frmt () ->
      Fmt.(
        pf
          frmt
          "@[<hov 2>Counterexample terms:@;@[<hov 2>%a@]"
          (list ~sep:comma Term.pp_term)
          (Set.elements (Set.diff t_set lstate.t_set))))
;;

let show_summary (spec_fname, repr_fname, target_fname) target_f =
  Log.info
    Fmt.(
      fun fmt () ->
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
  Log.info
    Fmt.(fun fmt () -> pf fmt "%a" (box (PMRS.pp ~short:(not !Config.verbose))) pmrs)
;;

(* Messages from the Lemmas  *)

let no_spurious_ctex () =
  Log.info
    Fmt.(fun fmt () -> pf fmt "All counterexamples are non-spurious: nothing to refine.")
;;

let spurious_violates_requires i =
  Log.info Fmt.(fun fmt () -> pf fmt "%i counterexamples violate requires." i)
;;

let print_infeasible_message t_set =
  Log.info
    Fmt.(
      fun frmt () ->
        pf
          frmt
          "@[<hov 2>This problem has no solution. Counterexample set:@;%a@]"
          (list ~sep:sp Term.pp_term)
          (Set.elements t_set))
;;

let announce_new_lemma_synthesis (det : term_state_detail) =
  Log.debug (fun f () ->
      Fmt.(
        match det.current_preconds with
        | None ->
          pf
            f
            "Synthesizing a new lemma candidate for term@;@[%a[%a]@]."
            pp_term
            det.term
            pp_subs
            det.recurs_elim
        | Some pre ->
          pf
            f
            "Synthesizing a new lemma candidate for term@;\
             @[%a[%a]@]@;\
             with precondition@;\
             @[%a@]"
            pp_term
            det.term
            pp_subs
            det.recurs_elim
            pp_term
            pre))
;;
