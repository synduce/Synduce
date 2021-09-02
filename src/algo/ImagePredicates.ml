open AState
open Base
open Lang
open Term
open Utils

let synthesize ~(p : psi_def) (positives : ctex list) (negatives : ctex list) =
  let _ = p in
  let _ = (positives, negatives) in
  Log.debug_msg "Synthesize predicates..";
  let vals ctex =
    List.iter ctex.ctex_eqn.eelim ~f:(fun (_, elimv) ->
        let tval = Eval.in_model ctex.ctex_model elimv in
        Log.debug_msg
          Fmt.(str "%a should not be in the image of %s" pp_term tval p.psi_reference.pvar.vname))
  in
  List.iter ~f:vals negatives;

  ()