open AState
open Base
open Lang
open Term
open Utils
open Smtlib
open SmtInterface
module Solvers = SyncSmt
open Syguslib.Sygus
open SygusInterface

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
  let out_term = mk_composite_base_type !_alpha in
  let atoms = Analysis.free_variables out_term in
  let iterations = ref 0 in
  let z3 = Solvers.make_z3_solver () in
  Solvers.load_min_max_defs z3;
  Solvers.declare_all z3 (List.map ~f:snd (SmtInterface.declare_datatype_of_rtype !_alpha));
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

let handle_ensures_synth_response (resp : solver_response option) (var : variable) =
  let parse_synth_fun (fname, _fargs, _, fbody) =
    let body, _ = infer_type (term_of_sygus (VarSet.to_env (VarSet.of_list [ var ])) fbody) in
    (fname, [], body)
  in
  match resp with
  | Some (RSuccess resps) ->
      let soln = List.map ~f:parse_synth_fun resps in
      (* let _ = List.iter ~f:(fun (s, vs, t) -> log_soln s vs t) soln in *)
      Some soln
  | Some RInfeasible | Some RFail | Some RUnknown | None -> None

let make_ensures_name (id : int) = "ensures_" ^ Int.to_string id

let synthfun_ensures ~(p : psi_def) (id : int) : command * variable * string =
  let var = Variable.mk ~t:(Some p.psi_reference.poutput_typ) (Alpha.fresh ()) in
  let params = [ (var.vname, sort_of_rtype p.psi_reference.poutput_typ) ] in
  let ret_sort = sort_of_rtype RType.TBool in
  let opset =
    List.fold ~init:OpSet.empty
      ~f:(fun acc func -> Set.union acc (Analysis.operators_of func.f_body))
      (PMRS.func_of_pmrs p.psi_reference @ PMRS.func_of_pmrs p.psi_repr
      @ match p.psi_tinv with None -> [] | Some pmrs -> PMRS.func_of_pmrs pmrs)
  in
  (* OpSet.of_list [ Binary Binop.Mod ] in *)
  let grammar = Grammars.generate_grammar ~guess:None ~bools:true opset params ret_sort in
  let logic = dt_extend_base_logic (logic_of_operators opset) in
  (CSynthFun (make_ensures_name id, params, ret_sort, grammar), var, logic)

let constraint_of_neg (id : int) ~(p : psi_def) (ctex : ctex) : command =
  ignore p;
  let params =
    List.concat_map
      ~f:(fun (_, elimv) -> [ Eval.in_model ctex.ctex_model elimv ])
      ctex.ctex_eqn.eelim
  in
  CConstraint
    (SyApp
       ( IdSimple "not",
         [ SyApp (IdSimple (make_ensures_name id), List.map ~f:sygus_of_term params) ] ))

let constraint_of_pos (id : int) (term : term) : command =
  CConstraint (SyApp (IdSimple (make_ensures_name id), [ sygus_of_term term ]))

let synthesize ~(p : psi_def) (positives : ctex list) (negatives : ctex list) : term option =
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
          (list ~sep:comma pp_term) positives);
  let id = 0 in
  let synth_objs, var, logic = synthfun_ensures ~p id in
  let neg_constraints = List.map ~f:(constraint_of_neg id ~p) negatives in
  let pos_constraints = List.map ~f:(constraint_of_pos id) positives in
  let extra_defs = [ max_definition; min_definition ] in
  let commands =
    CSetLogic logic
    :: (extra_defs @ [ synth_objs ] @ neg_constraints @ pos_constraints @ [ CCheckSynth ])
  in
  match
    handle_ensures_synth_response (Syguslib.Solvers.SygusSolver.solve_commands commands) var
  with
  | None -> None
  | Some solns ->
      let _, _, body = List.nth_exn solns 0 in
      Some (mk_fun [ FPatVar var ] body)
