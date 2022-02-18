open Smtlib
module Stats : Solvers.Statistics
module SmtLog : Solvers.Logger

module AsyncSmt : sig
  type response = SmtLib.solver_response Lwt.t

  type solver = Solvers.Asyncs(SmtLog)(Stats).solver =
    { s_name : string
    ; s_pinfo : Lwt_process.process
    ; s_pid : int
    ; s_inputc : Lwt_io.output_channel
    ; s_outputc : Lwt_io.input_channel
    ; mutable s_scope_level : int
    ; s_declared : (string, int) Base.Hashtbl.t
    ; s_log_file : string
    ; s_log_outc : Stdio.Out_channel.t
    }

  val async_online_solvers : (int * solver) list ref
  val _last_resp : (int, int * Sexplib0.Sexp.t) Base.Hashtbl.t
  val solver_verbose_response_summary : solver -> Sexplib0.Sexp.t -> unit
  val solver_verbose_pp_last_resp_count : solver -> unit
  val already_declared : solver -> SmtLib.smtSymbol -> bool
  val solver_declare : solver -> SmtLib.smtSymbol -> unit
  val open_log : unit -> unit
  val log : ?solver:solver option -> SmtLib.command -> unit
  val exec_command : solver -> SmtLib.command -> response
  val solver_make_cancellable : solver -> 'a Lwt.t -> unit
  val close_solver : solver -> unit Lwt.t
  val check_sat : solver -> response
  val exec_all : solver -> SmtLib.command list -> unit Lwt.t
  val get_model : solver -> response
  val load_min_max_defs : solver -> unit Lwt.t
  val set_logic : solver -> Logics.logic -> unit Lwt.t
  val set_option : solver -> string -> string -> unit Lwt.t
  val smt_assert : solver -> SmtLib.smtTerm -> unit Lwt.t
  val spop : solver -> unit Lwt.t
  val spush : solver -> unit Lwt.t

  val cancellable_task
    :  solver * int Lwt.t * int Lwt.u
    -> (solver * int Lwt.t -> 'b Lwt.t)
    -> 'b Lwt.t * int Lwt.u

  val make_solver : string -> solver * int Lwt.t * int Lwt.u
end

module SyncSmt : sig
  type solver_response = SmtLib.solver_response

  val is_sat : solver_response -> bool
  val is_unsat : solver_response -> bool

  type online_solver = Solvers.Synchronous(SmtLog)(Stats).online_solver =
    { s_name : string
    ; s_pid : int
    ; s_inputc : out_channel
    ; s_outputc : in_channel
    ; mutable s_online : bool
    ; mutable s_scope_level : int
    ; s_declared : (string, int) Base.Hashtbl.t
    ; s_log_file : string
    ; s_log_outc : out_channel
    }

  val open_log : unit -> unit
  val log : ?solver:online_solver option -> SmtLib.command -> unit
  val already_declared : online_solver -> SmtLib.smtSymbol -> bool
  val solver_declare : online_solver -> SmtLib.smtSymbol -> unit
  val exec_command : online_solver -> SmtLib.command -> solver_response
  val close_solver : online_solver -> unit
  val call_solver : online_solver -> SmtLib.command list -> solver_response

  exception SolverError of Sexplib0.Sexp.t list

  val solver_response_errors : solver_response -> Sexplib0.Sexp.t list
  val pp_solver_response : Format.formatter -> solver_response -> unit
  val check_sat : online_solver -> solver_response
  val exec_all : online_solver -> SmtLib.command list -> unit
  val get_model : online_solver -> solver_response
  val load_min_max_defs : online_solver -> unit
  val set_logic : online_solver -> Logics.logic -> unit
  val set_option : online_solver -> string -> string -> unit
  val smt_assert : online_solver -> SmtLib.smtTerm -> unit
  val spop : online_solver -> unit
  val spush : online_solver -> unit
  val make_z3_solver : unit -> online_solver
  val make_yices_solver : unit -> online_solver
  val make_cvc_solver : unit -> online_solver
  val make_solver : string -> online_solver
end

val rtype_of_smtSort : SmtLib.smtSort -> RType.t option
val sort_of_rtype : RType.t -> SmtLib.smtSort
val dec_parametric : RType.t -> RType.t list -> SmtLib.smtSort
val decl_of_tup_type : RType.t list -> SmtLib.smtSymbol * SmtLib.command
val declare_datatype_of_rtype : RType.t -> (SmtLib.smtSymbol list * SmtLib.command) list
val smtPattern_of_pattern : Term.pattern -> SmtLib.smtPattern
val term_of_const : Term.Constant.t -> SmtLib.smtTerm
val smt_of_term : Term.term -> SmtLib.smtTerm
val smt_of_case : Term.match_case -> SmtLib.match_case
val constant_of_smtConst : SmtLib.smtSpecConstant -> Term.Constant.t

type id_kind =
  | ICstr of string
  | IVar of Term.variable
  | IBinop of Term.Binop.t
  | IUnop of Term.Unop.t
  | IBool of bool
  | ITupCstr
  | INotDef

val term_of_smt
  :  (string, Term.variable, Base.String.comparator_witness) Base.Map.t
  -> SmtLib.smtTerm
  -> Term.term

val sorted_vars_of_vars : Term.VarSet.t -> SmtLib.smtSortedVar list

type term_model = (string, Term.term, Base.String.comparator_witness) Base.Map.t

val model_to_constmap
  :  SyncSmt.solver_response
  -> (string, Term.term, Base.String.comparator_witness) Base.Map.t

val model_to_varmap : Term.VarSet.t -> SyncSmt.solver_response -> Term.term Term.VarMap.t

val request_different_models
  :  term_model
  -> int
  -> Solvers.Synchronous(SmtLog)(Stats).online_solver
  -> (string, Term.term, Base.String.comparator_witness) Base.Map.t list

val request_different_models_async
  :  term_model Lwt.t
  -> int
  -> Solvers.Asyncs(SmtLog)(Stats).solver
  -> (string, Term.term, Base.String.comparator_witness) Base.Map.t list Lwt.t

val smtPattern_of_term : Term.term -> SmtLib.smtPattern option
val mk_assert : SmtLib.smtTerm -> SmtLib.command
val smt_of_pmrs : PMRS.t -> SmtLib.command list

module Commands : sig
  val decls_of_vars : Term.VarSet.t -> SmtLib.command list
  val decl_of_tuple_type : RType.t -> SmtLib.command list

  (**
    Produces a list of commands to be used in the preamble of a solver session.
    @param logic a SMT logic provided as a string.
    @param induction is set to false by default. If set to true, then the quant-ind option
    of the solver is set of true and if !Config.induction_proof_tlimit is positive, then
    the tlimit option of the solver is used to set a timeout on exection of the proof.
    @param incremental set to false by default.
    @param load_defs is set to true by default, and the definitions of min and max are added
    to the solver.
    @param models is set to true by default. Set to false if you don't need models during the
    session.
    @param proofs is set to false by default. Set to true if you want to extract proofs from the
    solver.
 *)
  val mk_preamble
    :  logic:Logics.logic
    -> ?induction:bool
    -> ?incremental:bool
    -> ?load_defs:bool
    -> ?models:bool
    -> ?proofs:bool
    -> unit
    -> SmtLib.command list

  val mk_def_fun
    :  string
    -> (string * RType.t) list
    -> RType.t
    -> Term.term
    -> SmtLib.command
end
