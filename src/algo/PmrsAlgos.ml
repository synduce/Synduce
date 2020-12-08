open Base
open Lang
open Lang.Term
open Utils



(* ============================================================================================= *)
(*                                                                                               *)
(* ============================================================================================= *)


let psi (target_f : PMRS.t) (orig_f : PMRS.t) (repr : PMRS.t) =
  let _ = target_f in
  let _ = orig_f in
  let _ = repr in
  let mgts = MGT.most_general_terms target_f in
  let xt =
    List.map mgts
      ~f:(fun ((xi_id, rule_id), t) -> (xi_id, rule_id), Option.map ~f:Analysis.expand_once t)
  in
  List.iter xt
    ~f:(fun (_, t) -> Fmt.(pf stdout "@[<hov 2>%a@]@." (option (list ~sep:comma Term.pp_term)) t))


(* ============================================================================================= *)
(*                                                 MAIN ENTRY POINTS                             *)
(* ============================================================================================= *)

let no_synth () = failwith "No synthesis objective found."

let solve_problem (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    (functions : (string, PMRS.top_function, Base.String.comparator_witness) Map.t) : unit =
  let orig_fname = "spec" and target_fname = "target" and repr_fname = "repr" in
  let repr, theta_to_tau =
    match Map.find pmrs repr_fname with
    | Some pmrs -> Either.First pmrs, Variable.vtype_or_new pmrs.pmain_symb
    | None ->
      (match Map.find functions repr_fname  with
       | Some (f,a,b) -> Either.Second (f,a,b), Variable.vtype_or_new f
       | None -> Log.error_msg "No representation function given."; no_synth ())
  in
  let orig_f, tau =
    match Map.find pmrs orig_fname with
    | Some pmrs -> pmrs, pmrs.pinput_typ
    | None -> Log.error_msg "No origin function found."; no_synth ()
  in
  let target_f, xi, theta =
    let target_f =
      match Map.find pmrs target_fname with
      | Some pmrs -> pmrs
      | None -> Log.error_msg "No target recursion scheme found"; no_synth ()
    in
    target_f, target_f.pparams, target_f.pinput_typ
  in
  (* Match origin and target recursion scheme types. *)
  (match theta_to_tau with
   | RType.TFun (theta', tau') ->
     let sb1 = RType.unify_one theta' theta in
     let sb2 = RType.unify_one tau tau' in
     (match sb1, sb2 with
      | Some sb1, Some sb2 ->
        (match RType.unify (RType.mkv (sb1 @ sb2)) with
         | Some sb' -> Term.Variable.update_var_types (RType.mkv sb')
         | None -> Log.error_msg "Could not unify θ and τ in problem definition.")
      | _ -> Log.error_msg "Could not unify θ and τ in problem definition.")
   | _ -> Log.error_msg "Representation function should be a function.");
  Term.(
    match Variable.vtype_or_new orig_f.pmain_symb, Variable.vtype_or_new target_f.pmain_symb with
    | TFun(_, tout), TFun(_, tout') ->
      (match RType.unify_one tout tout' with
       | Some subs -> Variable.update_var_types (RType.mkv subs)
       | None -> Log.error_msg "Failed to unify output types."; no_synth ())
    |_ -> Log.error_msg "Original or target is not a function."; no_synth ());
  (*  Update the type of all the components. *)
  let repr =
    match repr with
    | Either.First pmrs -> Either.First (PMRS.infer_pmrs_types pmrs)
    | Either.Second (f,a,b) ->
      let b', _ = Term.infer_type b in Either.Second (f, a, b')
  in
  let target_f = PMRS.infer_pmrs_types target_f in
  let orig_f = PMRS.infer_pmrs_types orig_f in
  let theta = target_f.pinput_typ in
  let t_out =
    Term.(match Variable.vtype_or_new orig_f.pmain_symb with
        | TFun(_, tout) -> tout
        | _ -> failwith "Unexpected.")
  in
  (* Print summary information about the problem. *)
  Log.info
    Fmt.(fun fmt () -> pf fmt " Ψ (%a) := ∀ x : %a. (%s o %s)(x) = %s(x)"
            (list ~sep:comma Term.Variable.pp) (Set.elements xi) RType.pp theta
            orig_fname repr_fname target_fname);
  Log.info Fmt.(fun fmt () -> pf fmt "%a" PMRS.pp orig_f);
  Log.info Fmt.(fun fmt () -> pf fmt "%a" PMRS.pp target_f);
  Log.info Fmt.(fun fmt () ->
      match repr with
      | Either.First pmrs -> pf fmt "%a" PMRS.pp pmrs
      | Either.Second (fv, args, body) ->
        pf fmt "%s(%a) = %a" fv.vname
          (list ~sep:comma Term.Variable.pp) args Term.pp_term body);
  let repr_pmrs =
    match repr with
    | Either.First p -> p
    | Either.Second (f,a,b) -> PMRS.func_to_pmrs f a b
  in
  AState.tau := tau;
  AState.theta := theta;
  AState.alpha := t_out;
  psi target_f orig_f repr_pmrs
