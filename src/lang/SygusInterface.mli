open Base
open Term
open Syguslib

module SygusSolver : sig
  module CoreSolver : sig
    type t =
      | CVC
      | DryadSynth
      | EUSolver

    val default_solver : t ref
  end

  val solve_commands
    :  ?timeout:float option
    -> ?solver_kind:CoreSolver.t
    -> Sygus.program
    -> Sygus.solver_response option Lwt.t * int Lwt.u
end

(**
    Translates a sygus sort to a type.
    Returns Some type if the sort corresponds to a valid type, otherwise None.
 *)
val rtype_of_sort : ctx:Context.t -> Sygus.sygus_sort -> RType.t option

(**
    Translate a type to a sygus sort.
    If the type is a type variable, it is replaced by the sort Int.
    A term of unassigned type appearing in a constraint should mean the
    type does not matter, and it can be soundly replaced by Int.
 *)
val sort_of_rtype : ctx:Context.t -> RType.t -> Sygus.sygus_sort

(**
    Returns true if the type argument requires the logic to include datatypes to reason about
    objects of that type.
 *)
val requires_dt_theory : ctx:Context.t -> RType.t -> bool

(**
    Returns true if the type argument requires the logic to include sets to reason about
    objects of that type.
 *)
val requires_set_theory : ctx:Context.t -> RType.t -> bool

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
val sygus_of_term : ctx:Context.t -> term -> Sygus.sygus_term

(**
    Translates a sygus-term to a term in an environment.
    The environment is a map from variable name to variable.
 *)
val term_of_sygus
  :  fctx:PMRS.Functions.ctx
  -> ctx:Context.t
  -> (string, variable, String.comparator_witness) Map.t
  -> Sygus.sygus_term
  -> term

(** Returns a list of terms that corresponds to the Skeleton interpreted as a grammar. *)
val grammar_production_of_skeleton
  :  Skeleton.t
  -> ints:Sygus.sygus_term
  -> bools:Sygus.sygus_term
  -> (Sygus.sygus_term * Sygus.sygus_sort) list
  -> Sygus.sygus_term list

(**
    Given a list of types, returns a list of sorted sygus variables.
    Returns as many variables as there are elements in the input lists,
    and a fresh name is used for each created variable.
*)
val sorted_vars_of_types : ctx:Context.t -> RType.t list -> Sygus.sorted_var list

(** Create the list of sorted vars corresponding to the list of variables,
  in the same order.
  *)
val sorted_vars_of_vars : ctx:Context.t -> variable list -> Sygus.sorted_var list

(** Create a function to synthesize from its name, arguments, return type and optional grammar.  *)
val mk_synthfun
  :  ctx:Context.t
  -> string
  -> variable list
  -> RType.t
  -> Sygus.grammar_def option
  -> Sygus.command

(** Create an invariant to synthesize from its name, arguments and optional grammar.  *)
val mk_synthinv
  :  ctx:Context.t
  -> string
  -> variable list
  -> Sygus.grammar_def option
  -> Sygus.command

(**
    Bind to the response of a solver to that if that response is a failure, the promise is
    deferrred by [!Config.wait_parallel_tlimit].
*)
val wait_on_failure
  :  int ref
  -> (Sygus.solver_response * 'a) Lwt.t
  -> (Sygus.solver_response * 'a) Lwt.t

module HLSolver : sig
  type t =
    { declared : string Hash_set.t
    ; logic : string
    ; constraints : Sygus.command list
    ; extra_defs : Sygus.command list
    ; definitions : Sygus.command list
    ; sorts : Sygus.command list
    ; objs : Sygus.command list
    }

  val make : ?extra_defs:Sygus.command list -> unit -> t
  val synthesize : Sygus.command list -> t -> t
  val constrain : ctx:Context.t -> term list -> t -> t
  val set_logic : string -> t -> t

  val solve
    :  ?timeout:float option
    -> ctx:Context.t
    -> t
    -> Sygus.solver_response option Lwt.t * int Lwt.u

  val to_file : ctx:Context.t -> string -> t -> unit
end

(**
  [pp_response] pretty prints Sygus solver responses.
*)
val pp_response : Formatter.t -> Sygus.solver_response -> unit
