open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils

(**
  AState: This module contains state variables for the synthesis algorithms, as well as type
   definitions and printing functions for displaying solutions.
 *)

type psi_def = {
  psi_target : PMRS.t;  (**
  The target recursion skeleton in the problem.
  *)
  psi_reference : PMRS.t;  (**
   The reference function in the problem.
  *)
  psi_repr : PMRS.t;  (**
    The representation function in the problem.
  *)
  psi_tinv : PMRS.t option;  (**
  The requires predicate of the target recursion skeleton.
  *)
  psi_repr_is_identity : bool;  (**
  A boolean that is true iff psi_repr is identity.
  *)
  psi_lifting : RType.t list;
      (** The current lifting: the output type of psi_target
  should be !_alpha extended with the tuple of types psi_lifting. *)
}

(**
  psi_def is the definition of the problem: the goal is to synthesize the unknowns in target such
  that target = orig ∘ repr
*)

(* State variables for the algorithm. *)

(** The type τ in the paper, input type of the reference function.  *)
let _tau = ref RType.TInt

(** The type θ in the paper, input type of the target recursion skeleton.  *)
let _theta = ref RType.TInt

(** The type D in the paper: the output type of the reference and the target recursion skeleton. The
    first element is the pure type output type of the functions, and the second element is an
    optional term that represents the additional predicate on the output of the reference function.
    The term is assumed to define a function (fun (free variables of term) -> term).
*)
let _alpha : (RType.t * Term.term option) ref = ref (RType.TInt, None)

(** Not useful for now. *)
let _span = ref 1

let refinement_steps = ref 0

(* ============================================================================================= *)
(*       Types for intermediate representations of solutions, lemmas, counterexamples, etc.      *)
(* ============================================================================================= *)

type soln = {
  soln_rec_scheme : PMRS.t;  (** The recursion scheme this object is a solution of. *)
  soln_implems : (symbol * variable list * term) list;
      (** The association list containing the implementation of each unknown. Each element of the
      list is a triple of unknown name, list of arguments, and body of the unknown implementation.
  *)
}
(** Representation of a solution, i.e. a function (as a PMRS) together with an association list
from unknown name to its implementation.
*)

type equation = {
  eterm : term;  (** The term from which the equation originates. *)
  eprecond : term option;  (** An optional precondition to the equation. *)
  elhs : term;  (** The left-hand side of the equation, containing no unknowns. *)
  erhs : term;  (** The right-hand side of the equation, possibly with unknowns. *)
  eelim : (term * term) list;  (** The substitution used to eliminate recursion. *)
}
(**
  Represents an equational constraint.
*)

type ctex = {
  ctex_eqn : equation;  (** The equation the counterexample relates to. *)
  ctex_vars : VarSet.t;  (** The variables appearing in the model. *)
  ctex_model : (int, term, Int.comparator_witness) Map.t;
      (** The model of the counterexample, mapping variable ids to terms. The terms should be
        constants.*)
}
(** A counterexample related to an equation.
*)

type equation_system = equation list

type lemma = { lem_map : (term, term, Terms.comparator_witness) Map.t }
(*
  A type for lemmas.
  For now, it only contains a map from terms to terms.
  It is used during the generation of constraints from a set of terms.
  If a term t used to generate a constraint appears in the lemma map, then the lemmas lem_map(t)
  are added to the precondition of the constraint, and every subterm of the form
  (f(r(x)) or (target(x)) is replaced by a scalar (matching the substitution happending
  during the constraint generation).
 *)

type lifting = { tmap : (term, term list, Terms.comparator_witness) Map.t }
(**
  The type to describe liftings.
  tmap is a map from terms to the expression of the lifting.
*)

type refinement_loop_state = {
  t_set : TermSet.t;
  u_set : TermSet.t;
  lemma : lemma;
  lifting : lifting;
}
(** The state of the main refinement loop. Currently, it is entirely determined by the sets T and U,
  accompanied with a set of lemmas that can be used during the generation of constraints.
 *)

(* ============================================================================================= *)
(*          Pretty printing functions.                                                           *)
(* ============================================================================================= *)

let pp_equation (f : Formatter.t) (eqn : equation) =
  match eqn.eprecond with
  | Some inv ->
      Fmt.(
        pf f "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a@;@[%a@;%a@;%a@]@]@]"
          (styled `Italic pp_term)
          eqn.eterm pp_subs eqn.eelim pp_term inv
          (styled (`Fg `Red) string)
          "=>" pp_term eqn.elhs
          (styled (`Fg `Red) string)
          "=" pp_term eqn.erhs)
  | None ->
      Fmt.(
        pf f "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a %a@]@]"
          (styled `Italic pp_term)
          eqn.eterm pp_subs eqn.eelim pp_term eqn.elhs
          (styled (`Fg `Red) string)
          "=" pp_term eqn.erhs)

let pp_ctex (f : Formatter.t) (ctex : ctex) : unit =
  let pp_model frmt model =
    (* Print as comma-separated list of variable -> term *)
    Fmt.(list ~sep:comma (pair ~sep:Utils.rightarrow (option Variable.pp) pp_term))
      frmt
      (List.map ~f:(fun (vid, t) -> (VarSet.find_by_id ctex.ctex_vars vid, t)) (Map.to_alist model))
  in
  Fmt.pf f "@[M = [%a]@]@;@[for %a@]@;@[with elim. %a@]" pp_model ctex.ctex_model pp_term
    ctex.ctex_eqn.eterm pp_subs ctex.ctex_eqn.eelim

let pp_implems (frmt : Formatter.t) (implems : (symbol * variable list * term) list) =
  let pp_single_or_tup frmt l =
    match l with
    | [] -> ()
    | [ (_, v) ] -> pp_term frmt v
    | _ :: _ ->
        Fmt.(pf frmt "(%a)" (list ~sep:(fun _f () -> pf _f ", ") pp_term) (List.map ~f:second l))
  in

  let pp_implem frmt (s, args, t) =
    let f v =
      match Variable.vtype_or_new v with
      | TTup tl ->
          let f i typ =
            let v' = Variable.mk ~t:(Some typ) (Alpha.fresh ~s:"j" ()) in
            (mk_sel (mk_var v) i, mk_var v')
          in
          List.mapi ~f tl
      | _ -> [ (mk_var v, mk_var v) ]
    in
    let subs = List.map ~f args in
    let t' = substitution (List.concat subs) t in
    Fmt.(
      pf frmt "@[<hov 2>%a %a @[%a@] %a@;%a@]"
        (styled (`Fg `Red) string)
        "let"
        (styled (`Fg `Cyan) string)
        s (list ~sep:sp pp_single_or_tup) subs
        (styled (`Fg `Red) string)
        "=" pp_term t')
  in
  Fmt.((list ~sep:(fun _f () -> pf _f "@.@.") pp_implem) frmt implems)

let pp_soln ?(use_ocaml_syntax = false) (frmt : Formatter.t) (solution : soln) =
  Fmt.(
    pf frmt "@.%a@.@.@[%a@]@." pp_implems solution.soln_implems
      (if use_ocaml_syntax then PMRS.pp_ocaml else PMRS.pp)
      solution.soln_rec_scheme)
