open AState
open Base
open Lang
open Term
open Utils
open Smtlib
open SmtInterface

let _NUM_POSIIVE_EXAMPLES_ = 30

let _POSITIVE_EXAMPLES_ : (int, term list) Hashtbl.t = Hashtbl.create (module Int) ~size:5

let add_positive_example (f : Variable.t) (ex : term) : unit =
  Hashtbl.add_multi ~key:f.vid ~data:ex _POSITIVE_EXAMPLES_

let get_positive_examples (f : Variable.t) : term list =
  Hashtbl.find_multi _POSITIVE_EXAMPLES_ f.vid

(** Generate positive examples for the input PMRS, using a SMT solver to find
  different possible outputs.
  *)
let gen_pmrs_positive_examples (p : PMRS.t) =
  let ref_typ_out = List.last_exn p.pinput_typ in
  let reference t = Reduce.reduce_term (Reduce.reduce_pmrs p t) in
  let out_term = mk_composite_base_type (fst !_alpha) in
  let atoms = Analysis.free_variables out_term in
  let iterations = ref 0 in
  let z3 = Solvers.make_z3_solver () in
  Solvers.load_min_max_defs z3;
  Solvers.declare_all z3 (List.map ~f:snd (SmtInterface.declare_datatype_of_rtype (fst !_alpha)));
  Solvers.declare_all z3 (decls_of_vars atoms);
  let mk_ex _ t =
    let t' = reference t in
    let fv = Analysis.free_variables t' in
    Solvers.spush z3;
    Solvers.declare_all z3 (decls_of_vars fv);
    Solvers.smt_assert z3 (smt_of_term (mk_bin Binop.Eq t' out_term));
    let resp =
      match Solvers.check_sat z3 with
      | SmtLib.Sat ->
          (* SAT: get the model. *)
          let mmap = model_to_varmap atoms (Solvers.get_model z3) in
          let value = Eval.in_model mmap out_term in
          (* Add the positive example for the current function. *)
          add_positive_example p.pvar value;
          (* Pop the current stack. *)
          Solvers.spop z3;
          (* Make the values forbidden in subsequent solver calls. *)
          Solvers.smt_assert z3 SmtLib.(mk_not (mk_eq (smt_of_term out_term) (smt_of_term value)));
          SmtLib.Sat
      | _ as t ->
          (* UNSAT: the loop will stop, but pop the current stack before.  *)
          Solvers.spop z3;
          t
    in
    resp
  in
  let _ =
    Expand.expand_loop iterations (* Stop at _NUM_POSITIVES_EXAMEPLES_ examples. *)
      ~r_stop:(fun _ -> !iterations > _NUM_POSIIVE_EXAMPLES_)
      mk_ex
      (TermSet.singleton (mk_var (Variable.mk ~t:(Some ref_typ_out) (Alpha.fresh ()))))
  in
  Solvers.close_solver z3;
  get_positive_examples p.pvar

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
  let positives = gen_pmrs_positive_examples p.psi_reference in
  Log.debug
    Fmt.(
      fun fmt () ->
        pf fmt "These examples are in the image of %s:@;%a" p.psi_reference.pvar.vname
          (list ~sep:comma pp_term) positives)
