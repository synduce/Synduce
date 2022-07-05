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

type witness_stat =
  | Valid
  | Spurious of spurious_cause list
  | Unknown
      (**
  A counterexample is either valid, or spurious with some reason, or unknown.
*)

type repair =
  | Lift
  | AddRecursiveCalls of (term * variable * term) list
  | NoRepair
[@@deriving sexp]

(** A counterexample related to an equation and some info on the validity of the counterexample.
*)
type witness =
  { witness_eqn : equation (** The equation the counterexample relates to. *)
  ; witness_vars : VarSet.t (** The variables in the model.*)
  ; witness_model : term VarMap.t
        (** The model of the counterexample, mapping variables to terms. The terms should be
        constants. *)
  ; witness_stat : witness_stat (** The spuriousness status of the counterexample. *)
  }

(** A counterexample to realizability is a pair of models: a pair of maps from variable ids to terms. *)
type unrealizability_witness =
  { i : int
  ; j : int
  ; ci : witness
  ; cj : witness
  }

type equation_system = equation list

(** Contains the last equation system used to solved the problem when a solution is found. *)
let solved_eqn_system : equation_system option ref = ref None

type cond_lemma =
  { cl_flag : bool
        (** Set to false if the info needs refinement (i.e. the
      set of witnesses and the lemmas are not in sync). *)
  ; cl_cond : term option
        (** A condition that can be assumed for the property in this info. *)
  ; cl_lemmas : term list
        (** A set of lemmas that is true about a term, under the assumption
            that the splitter predicate is true.*)
  ; cl_negatives : term VarMap.t list
        (** A set of valuations for the term's evaluated symbolic value that leads to
      the lemma being false. *)
  ; cl_positives : term VarMap.t list
        (** A set of valuations for the term's evaluated symbolic value that leads to
      the lemma being true. *)
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
  | Unrealizable of repair * unrealizability_witness list
  | Failed of string * 'a
