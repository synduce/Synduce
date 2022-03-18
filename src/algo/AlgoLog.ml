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
    let json =
      `Assoc
        [ "progress", LogJson.refinement_steps_summary ()
        ; "verif_elapsed", `Float !Stats.verif_time
        ]
    in
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

let show_counterexamples ~(ctx : Context.t) lstate t_set =
  Log.debug (fun frmt () ->
      pf
        frmt
        "@[<hov 2>Counterexample terms:@;@[<hov 2>%a@]"
        (list ~sep:comma (Term.pp_term ctx))
        (Set.elements (Set.diff t_set lstate.t_set)))
;;

let show_summary ~(ctx : Context.t) (spec_fname, repr_fname, target_fname) target_f =
  Log.info (fun fmt () ->
      pf
        fmt
        " Ψ (%a) := ∀ x : %a. (%a o %a)(x) = %a(x)"
        (list ~sep:comma (Term.Variable.pp ctx))
        (Set.elements target_f.PMRS.psyntobjs)
        (styled (`Fg `Yellow) (list ~sep:sp RType.pp))
        target_f.pinput_typ
        (styled (`Fg `Blue) string)
        spec_fname
        (styled (`Fg `Blue) string)
        repr_fname
        (styled (`Fg `Blue) string)
        target_fname)
;;

let show_pmrs ~(ctx : Context.t) pmrs =
  Log.info (fun fmt () ->
      pf fmt "%a" (box (PMRS.pp ~ctx ~short:(not !Config.verbose))) pmrs)
;;

let show_new_rskel ~(ctx : Context.t) i p =
  if !Config.Optims.max_solutions > 0
  then (
    Log.sep ~i:(Some i) ();
    Log.info (fun fmt () ->
        pf
          fmt
          "💁 Attempting to find solution to skeleton:@;<1 10>%a"
          (box (PMRS.pp ~ctx ~short:false))
          p.PsiDef.target))
;;

let msg_too_many_opts () =
  Log.info (fun fmt () ->
      pf
        fmt
        "@[It seems some optimizations caused an error.@;\
         Turning them off and trying again.@]")
;;

(* ============================================================================================= *)
(*                           Messages from the Counterexamples                                   *)
(* ============================================================================================= *)

let pp_unrealizability_ctex
    ~(ctx : Context.t)
    (frmt : Formatter.t)
    (uc : unrealizability_ctex)
    : unit
  =
  let pp_model frmt model =
    (* Print as comma-separated list of variable -> term *)
    Fmt.(list ~sep:comma (pair ~sep:Utils.rightarrow (Variable.pp ctx) (pp_term ctx)))
      frmt
      (Map.to_alist model)
  in
  Fmt.(
    pf
      frmt
      "@[M<%i> = [%a]@]@;@[M'<%i> = [%a]@]"
      uc.i
      pp_model
      uc.ci.ctex_model
      uc.j
      pp_model
      uc.cj.ctex_model)
;;

let show_unrealizability_witnesses ~(ctx : Context.t) unknowns eqns ctexs =
  Log.verbose (fun f () ->
      match ctexs with
      | [] ->
        Fmt.(
          pf
            f
            "(%a) no counterexample to realizability found."
            (VarSet.pp_var_names ctx)
            unknowns)
      | _ :: _ ->
        Fmt.(
          pf
            f
            "@[%a) Counterexamples found!@;\
             @[<hov 2>❔ Equations:@;\
             @[<v>%a@]@]@;\
             @[<hov 2>❔ Counterexample models:@;\
             @[<v>%a@]@]@]"
            (VarSet.pp_var_names ctx)
            unknowns
            (list ~sep:sp (box (pair ~sep:colon int (box (pp_equation ~ctx)))))
            (List.mapi ~f:(fun i eqn -> i, eqn) eqns)
            (list ~sep:sep_and (pp_unrealizability_ctex ~ctx))
            ctexs))
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

let print_infeasible_message ~ctx t_set =
  Log.info (fun frmt () ->
      pf
        frmt
        "@[<hov 2>This problem has no solution. Counterexample set:@;%a@]"
        (list ~sep:sp (Term.pp_term ctx))
        (Set.elements t_set))
;;

let announce_new_term_state ~(ctx : Context.t) ctex =
  Log.debug (fun fmt () ->
      pf
        fmt
        "Creating new term state for term@;%a@;under condition %a@;"
        (pp_term ctx)
        ctex.ctex_eqn.eterm
        (option (pp_term ctx))
        ctex.ctex_eqn.esplitter)
;;

let announce_new_lemma_synthesis
    ~(ctx : Context.t)
    (thread_no : int)
    (det : term_state_detail)
  =
  Log.debug (fun f () ->
      match det.current_preconds with
      | None ->
        pf
          f
          "[%i] Synthesizing a new lemma candidate for term@;@[%a[%a]@]."
          thread_no
          (pp_term ctx)
          det.term
          (pp_subs ctx)
          det.recurs_elim
      | Some pre ->
        pf
          f
          "[%i] Synthesizing a new lemma candidate for term@;\
           @[%a[%a]@]@;\
           with precondition@;\
           @[%a@]"
          thread_no
          (pp_term ctx)
          det.term
          (pp_subs ctx)
          det.recurs_elim
          (pp_term ctx)
          pre)
;;

let lemma_not_proved_correct (m : Stats.verif_method) =
  Log.verbose (fun f () ->
      pf
        f
        "This lemma has been proved incorrect by %s. Refining lemma..."
        (Stats.verif_method_to_str m))
;;

let lemma_proved_correct
    ~(ctx : Context.t)
    (proof_method : Stats.verif_method)
    (det : term_state_detail)
    (lemma_term : term)
  =
  let lemma_term = Eval.simplify lemma_term in
  Log.verbose (fun f () ->
      pf
        f
        "This lemma has been proven correct by %s."
        (Stats.verif_method_to_str proof_method));
  let lemma_str =
    str
      "(%a)[%a]->%s(%s)= %a"
      (pp_term ctx)
      det.term
      (list ~sep:comma (Fmt.pair ~sep:rightarrow (pp_term ctx) (pp_term ctx)))
      det.recurs_elim
      det.lemma.vname
      (String.concat ~sep:", " (List.map ~f:(fun v -> v.vname) det.scalar_vars))
      (box (pp_term ctx))
      lemma_term
  in
  Stats.set_lemma_synthesized
    "requires_lemma"
    (String.concat ~sep:" " (List.map ~f:String.strip (String.split_lines lemma_str)));
  Stats.set_last_lemma_proof_method proof_method;
  Log.info (fun frmt () ->
      pf
        frmt
        "Lemma for term %a:@;\"%s %s =@;@[%a@]\"."
        (pp_term ctx)
        det.term
        det.lemma.vname
        (String.concat ~sep:" " (List.map ~f:(fun v -> v.vname) det.scalar_vars))
        (box (pp_term ctx))
        lemma_term)
;;

(* Messages from ImagePredicates *)

let violates_ensures ~(ctx : Context.t) p ctexs =
  List.iter ctexs ~f:(fun ctex ->
      List.iter ctex.ctex_eqn.eelim ~f:(fun (_, elimv) ->
          let tval = Eval.in_model ~ctx ctex.ctex_model elimv in
          Log.verbose (fun fmt () ->
              pf
                fmt
                "%a should not be in the image of %s"
                (pp_term ctx)
                tval
                p.PsiDef.reference.pvar.vname)))
;;

let positives_ensures ~(ctx : Context.t) p positives =
  Log.verbose (fun fmt () ->
      pf
        fmt
        "@[These examples are in the image of %s:@;%a@]"
        p.PsiDef.reference.pvar.vname
        (list ~sep:comma (pp_term ctx))
        positives)
;;

let show_new_ensures_predicate ~(ctx : Context.t) (f : variable) (ensures : term) =
  let ensures = Reduce.reduce_term ensures in
  Stats.set_lemma_synthesized
    "ensures_lemma"
    (String.strip (str "%a" (pp_term ctx) ensures));
  Log.info
    Fmt.(
      fun fmt () ->
        let input_t =
          let in_t_l, _ = RType.fun_typ_unpack (Variable.vtype_or_new ctx f) in
          match in_t_l with
          | [ a ] -> a
          | _ -> RType.TTup in_t_l
        in
        pf
          fmt
          "@[Learned that ∀x : %a,@;@[%a(x) satifies@;%a@]@]"
          (styled (`Fg `Yellow) RType.pp)
          input_t
          (styled (`Fg `Blue) string)
          f.vname
          (box (pp_term ctx))
          ensures)
;;

(* ============================================================================================= *)
(*                  Messages from the Counterexamples Classification                             *)
(* ============================================================================================= *)

let image_ctex_class
    ~(ctx : Context.t)
    (p : PsiDef.t)
    (ctex : ctex)
    (resp : Smtlib.SmtLib.solver_response)
    (vmethod : Stats.verif_method)
  =
  Stats.update_counterexample_classification_method vmethod;
  Log.verbose (fun frmt () ->
      if SmtInterface.SyncSmt.is_unsat resp
      then
        Fmt.(
          pf
            frmt
            "%s: %a: ctex not in the image of reference function \"%s\":@;<1 4>%a."
            (Stats.verif_method_to_str vmethod)
            (styled (`Fg `Red) string)
            "SPURIOUS"
            p.PsiDef.reference.pvar.vname
            (box (pp_ctex ~ctx))
            ctex)
      else if SmtInterface.SyncSmt.is_sat resp
      then
        Fmt.(
          pf
            frmt
            "%s: %a: ctex in the image of %s:@;%a"
            (Stats.verif_method_to_str vmethod)
            (styled (`Fg `Green) string)
            "VALID"
            p.PsiDef.reference.pvar.vname
            (box (pp_ctex ~ctx))
            ctex)
      else
        Fmt.(
          pf
            frmt
            "%s: %a: Is the following in the image of %s?@;%a"
            (Stats.verif_method_to_str vmethod)
            (styled (`Fg `White) (styled (`Bg `Red) string))
            "UNKNOWN"
            p.PsiDef.reference.pvar.vname
            (box (pp_ctex ~ctx))
            ctex))
;;

let requires_ctex_class
    ~(ctx : Context.t)
    (tinv : PMRS.t)
    (ctex : ctex)
    (resp : Smtlib.SmtLib.solver_response)
    (vmethod : Stats.verif_method)
  =
  Stats.update_counterexample_classification_method vmethod;
  Log.verbose (fun frmt () ->
      if SmtInterface.SyncSmt.is_unsat resp
      then
        Fmt.(
          pf
            frmt
            "%s: %a ctex does not satisfy \"%s\":@;<1 4>%a"
            (Stats.verif_method_to_str vmethod)
            (styled (`Fg `Red) string)
            "SPURIOUS"
            tinv.PMRS.pvar.vname
            (box (pp_ctex ~ctx))
            ctex)
      else if SmtInterface.SyncSmt.is_sat resp
      then
        Fmt.(
          pf
            frmt
            "%s: %a ctex satisfies %s:@;<1 4>%a"
            (Stats.verif_method_to_str vmethod)
            (styled (`Fg `Green) string)
            "VALID"
            tinv.PMRS.pvar.vname
            (box (pp_ctex ~ctx))
            ctex)
      else
        Fmt.(
          pf
            frmt
            "%s: %a The ctex satisfies %s?@;<1 4>%a"
            (Stats.verif_method_to_str vmethod)
            (styled (`Fg `White) (styled (`Bg `Red) string))
            "UNKNOWN"
            tinv.PMRS.pvar.vname
            (box (pp_ctex ~ctx))
            ctex))
;;
