open Smtlib
module Stats : Solvers.Statistics
module SmtLog : Solvers.Logger

val is_unsat : SmtLib.solver_response -> bool
val is_sat : SmtLib.solver_response -> bool

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

  val make_solver : ?hint:string -> string -> solver * int Lwt.t * int Lwt.u
end

(** `wait_on_failure c s` waits if the first component of s indicates that the solver
    failed or returned unknown.
    Will stop waiting when `Config.wait_parallel_tlimit` seconds have elapsed or
    `c <= 1`. Decrements `c` when it stops waiting, or if the solver response indicates
    success.
*)
val wait_on_failure
  :  int ref
  -> (SmtLib.solver_response * 'a) Lwt.t
  -> (SmtLib.solver_response * 'a) Lwt.t

val rtype_of_smtSort : ctx:Term.Context.t -> SmtLib.smtSort -> RType.t option
val sort_of_rtype : RType.t -> SmtLib.smtSort
val dec_parametric : RType.t -> RType.t list -> SmtLib.smtSort
val decl_of_tup_type : RType.t list -> SmtLib.smtSymbol * SmtLib.command

val declare_datatype_of_rtype
  :  ctx:Term.Context.t
  -> RType.t
  -> (SmtLib.smtSymbol list * SmtLib.command) list

val smtPattern_of_pattern : ctx:Term.Context.t -> Term.pattern -> SmtLib.smtPattern
val term_of_const : Term.Constant.t -> SmtLib.smtTerm
val smt_of_term : ctx:Term.Context.t -> Term.term -> SmtLib.smtTerm
val smt_of_case : ctx:Term.Context.t -> Term.match_case -> SmtLib.match_case
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
  :  fctx:PMRS.Functions.ctx
  -> ctx:Term.Context.t
  -> (string, Term.variable, Base.String.comparator_witness) Base.Map.t
  -> SmtLib.smtTerm
  -> Term.term

val sorted_vars_of_vars : ctx:Term.Context.t -> Term.VarSet.t -> SmtLib.smtSortedVar list

type term_model = (string, Term.term, Base.String.comparator_witness) Base.Map.t

val model_to_constmap
  :  fctx:PMRS.Functions.ctx
  -> ctx:Term.Context.t
  -> SmtLib.solver_response
  -> (string, Term.term, Base.String.comparator_witness) Base.Map.t

val model_to_varmap
  :  fctx:PMRS.Functions.ctx
  -> ctx:Term.Context.t
  -> Term.VarSet.t
  -> SmtLib.solver_response
  -> Term.term Term.VarMap.t

val request_different_models
  :  fctx:PMRS.Functions.ctx
  -> ctx:Term.Context.t
  -> term_model
  -> int
  -> AsyncSmt.solver
  -> (string, Term.term, Base.String.comparator_witness) Base.Map.t list Lwt.t

val request_different_models_async
  :  fctx:PMRS.Functions.ctx
  -> ctx:Term.Context.t
  -> term_model Lwt.t
  -> int
  -> Solvers.Asyncs(SmtLog)(Stats).solver
  -> (string, Term.term, Base.String.comparator_witness) Base.Map.t list Lwt.t

val smtPattern_of_term : Term.term -> SmtLib.smtPattern option
val mk_assert : SmtLib.smtTerm -> SmtLib.command

(**
  Generate a set of definitions that define a PMRS. This can consist of datatype declarations
  and recursive definitions. If the PMRS uses parameters, then the definitions for the parameters
  are included in the set of definitions.
*)
val smt_of_pmrs
  :  fctx:PMRS.Functions.ctx
  -> ctx:Term.Context.t
  -> PMRS.t
  -> SmtLib.command list

module Commands : sig
  val decls_of_vars : ctx:Term.Context.t -> Term.VarSet.t -> SmtLib.command list
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
    :  ctx:Term.Context.t
    -> string
    -> (string * RType.t) list
    -> RType.t
    -> Term.term
    -> SmtLib.command
end
