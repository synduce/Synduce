open AState
open Base
open Lang
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
