open Base
module OC = Stdio.Out_channel
module IC = Stdio.In_channel

type solver_response = SmtLib.solver_response

val is_sat : solver_response -> bool

val is_unsat : solver_response -> bool

type online_solver = {
  s_name : string;
  s_pid : int;
  s_inputc : OC.t;
  s_outputc : IC.t;
  mutable s_scope_level : int;
  s_declared : (string, int) Base.Hashtbl.t;
  s_log_file : string;
  s_log_outc : OC.t;
}

val exec_command : online_solver -> SmtLib.command -> solver_response

val handle_sigchild : int -> unit

val make_solver : name:string -> string -> string array -> online_solver

val close_solver : online_solver -> unit

val call_solver : online_solver -> SmtLib.command list -> solver_response

val make_z3_solver : unit -> online_solver

val make_cvc4_solver : unit -> online_solver

val call_solver_default : online_solver option -> SmtLib.command list -> solver_response

exception SolverError of Sexplib0.Sexp.t list

val solver_response_errors : solver_response -> Sexplib0.Sexp.t list

val pp_solver_response : Formatter.t -> solver_response -> unit

val check_sat : online_solver -> solver_response

val declare_all : online_solver -> SmtLib.command list -> unit

val get_model : online_solver -> solver_response

val load_min_max_defs : online_solver -> unit

val set_logic : online_solver -> string -> unit

val set_option : online_solver -> string -> string -> unit

val smt_assert : online_solver -> SmtLib.smtTerm -> unit

val spop : online_solver -> unit

val spush : online_solver -> unit
