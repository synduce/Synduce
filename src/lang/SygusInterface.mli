open Base
open Term
open Syguslib

module SygusSolver : sig
  type t =
    | CVC
    | DryadSynth
    | EUSolver

  val solve_commands
    :  ?solver_kind:t
    -> Sygus.program
    -> Sygus.solver_response option Lwt.t * int Lwt.u

  val default_solver : t ref
end

(**
    Translates a sygus sort to a type.
    Returns Some type if the sort corresponds to a valid type, otherwise None.
 *)
val rtype_of_sort : Sygus.sygus_sort -> RType.t option

(**
    Translate a type to a sygus sort.
    If the type is a type variable, it is replaced by the sort Int.
    A term of unassigned type appearing in a constraint should mean the
    type does not matter, and it can be soundly replaced by Int.
 *)
val sort_of_rtype : RType.t -> Sygus.sygus_sort

(**
    Returns true if the type argument requires the logic to include datatypes to reason about
    objects of that type.
 *)
val requires_dt_theory : RType.t -> bool

(**
    Returns a string corresponding to the logic that allows to reason about all the operators
    in the set. Does not include the "DT" prefix required for reasoning about datatypes (see [requires_dt_theory]).
 *)
val logic_of_operators : ?nonlinear:bool -> OpSet.t -> string

(** Extend the base logic provided as a string to the same logic with datatypes.
*)
val dt_extend_base_logic : string -> string

(**
    Translates a term to a sygus term.
 *)
val sygus_of_term : term -> Sygus.sygus_term

(**
    Translates a sygus-term to a term in an environment.
    The environment is a map from variable name to variable.
 *)
val term_of_sygus
  :  (string, variable, String.comparator_witness) Map.t
  -> Sygus.sygus_term
  -> term

(**
    Given a set of variables, returns a list of commands that correspond to the
    sort declarations necessary of declare these variables.
    It is useful especially if the variables as of used-defined variant types.
 *)
val declare_sorts_of_vars : VarSet.t -> Sygus.command list

(**
    Returns a list of sygus commands of the form (declare-const v sort) from a set of variables.
*)
val declarations_of_vars : VarSet.t -> Sygus.command list

(**
    Given a list of types, returns a list of sorted sygus variables.
    Returns as many variables as there are elements in the input lists,
    and a fresh name is used for each created variable.
*)
val sorted_vars_of_types : RType.t list -> Sygus.sorted_var list

(**
    Bind to the response of a solver to that if that response is a failure, the promise is
    deferrred by [!Config.wait_parallel_tlimit].
*)
val wait_on_failure
  :  int ref
  -> (Sygus.solver_response * 'a) Lwt.t
  -> (Sygus.solver_response * 'a) Lwt.t
