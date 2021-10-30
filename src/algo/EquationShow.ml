open AState
open Base
open Lang.Term
open Utils

let show_invariants invariants =
  if Set.length invariants > 0
  then
    Log.verbose
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "Imᵢₙᵥ: @[<hov 2>%a@]"
            (styled `Italic (list ~sep:comma pp_term))
            (Set.elements invariants))
  else Log.verbose_msg "No image invariants."
;;

let show_equations tset eqns =
  Log.verbose (fun f () ->
      let print_less = List.take eqns !Config.pp_eqn_count in
      Fmt.(
        pf f "Equations (%i) @." (Set.length tset);
        List.iter ~f:(fun eqn -> Fmt.pf f "@[%a@]@." pp_equation eqn) print_less))
;;

let show_lifting_constraints lifting_eqns =
  Log.verbose (fun f () ->
      let print_less = List.take lifting_eqns !Config.pp_eqn_count in
      Fmt.(
        pf f "Lifting constraints (%i) @." (List.length lifting_eqns);
        List.iter ~f:(fun eqn -> Fmt.pf f "@[%a@]@." pp_equation eqn) print_less))
;;

let error_msg_comp_not_found s soln =
  Log.error
    Fmt.(
      fun fmt () ->
        pf
          fmt
          "Unexpected: did not find %s in %a."
          s.vname
          (list ~sep:sp (parens string))
          (List.map soln ~f:(fun (a, _, _) -> a)))
;;
