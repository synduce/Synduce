open Base
open Lang
open Lang.Term
open Syguslib.Sygus

(**
  Common.ProblemDefs: This module contains state variables for the synthesis algorithms, as well as type
   definitions and printing functions for displaying solutions.
 *)

module PsiDef = Psi

(* ============================================================================================= *)
(*       Types for intermediate representations of solutions, lemmas, counterexamples, etc.      *)
(* ============================================================================================= *)

(** Representation of a solution, i.e. a function (as a PMRS) together with an association list
from unknown name to its implementation.
*)
type soln =
  { soln_rec_scheme : PMRS.t (** The recursion scheme this object is a solution of. *)
  ; soln_implems : (symbol * variable list * term) list
        (** The association list containing the implementation of each unknown. Each element of the
      list is a triple of unknown name, list of arguments, and body of the unknown implementation.
  *)
  }

(**
  Represents an equational constraint.
*)
type equation =
  { eterm : term (** The term from which the equation originates. *)
  ; eprecond : term option (** An optional precondition to the equation. *)
  ; esplitter : term option
  ; elhs : term (** The left-hand side of the equation, containing no unknowns. *)
  ; erhs : term (** The right-hand side of the equation, possibly with unknowns. *)
  ; eelim : (term * term) list (** The substitution used to eliminate recursion. *)
  }

type spurious_cause =
  | ViolatesTargetRequires
  | NotInReferenceImage
      (** A counterexample can be spurious either because it wiolates the target's requires or
  because some values assigned to recursion elimination variables are not in the reference
  function's image.
*)

type ctex_stat =
  | Valid
  | Spurious of spurious_cause list
  | Unknown
      (**
  A counterexample is either valid, or spurious with some reason, or unknown.
*)

(** A counterexample related to an equation and some info on the validity of the counterexample.
*)
type ctex =
  { ctex_eqn : equation (** The equation the counterexample relates to. *)
  ; ctex_vars : VarSet.t (** The variables in the model.*)
  ; ctex_model : term VarMap.t
        (** The model of the counterexample, mapping variables to terms. The terms should be
        constants. *)
  ; ctex_stat : ctex_stat (** The spuriousness status of the counterexample. *)
  }

(** A counterexample to realizability is a pair of models: a pair of maps from variable ids to terms. *)
type unrealizability_ctex =
  { i : int
  ; j : int
  ; ci : ctex
  ; cj : ctex
  }

type equation_system = equation list

(** Contains the last equation system used to solved the problem when a solution is found. *)
let solved_eqn_system : equation_system option ref = ref None

(** A type containing info about a term and lemmas associated to it. *)
type term_info =
  { term : term (** The term the info is about. *)
  ; splitter : term option
        (** A condition that can be assumed for the property in this info. *)
  ; lemmas : term list
        (** A set of lemmas that is true about a term, under the assumption
            that the splitter predicate is true.*)
  ; lemma : variable (** A function variable to name the lemma. *)
  ; lemma_candidate : term option (** A lemma that is currently only a candidate.*)
  ; negative_ctexs : ctex list
        (** A set of valuations for the term's evaluated symbolic value that leads to
      the lemma being false. *)
  ; positive_ctexs : ctex list
        (** A set of valuations for the term's evaluated symbolic value that leads to
      the lemma being true. *)
  ; recurs_elim : (term * term) list (** The recursion elimination map for that term. *)
  ; scalar_vars : variable list
  ; current_preconds : term option
  }

(**
  The type to describe liftings.
  tmap is a map from terms to the expression of the lifting.
*)
type lifting = { tmap : ((int * term) * term) list }

(** The state of the main refinement loop. Currently, it is entirely determined by the sets T and U,
  accompanied with a set of lemmas that can be used during the generation of constraints.
 *)
type refinement_loop_state =
  { t_set : TermSet.t
  ; u_set : TermSet.t
  ; lifting : lifting
  ; assumptions : equation list
  }

(** The algorithm may answer with realizable, in which case it provides a solution, unrealizable,
in which case it provides a list of counterexamples, or failure.
*)
type 'a segis_response =
  | Realizable of soln
  | Unrealizable of unrealizability_ctex list
  | Failed of 'a
