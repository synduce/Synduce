open Lang
open Term
open Smtlib

val empty_term_state : AState.term_state

val set_term_lemma
  :  p:AState.psi_def
  -> AState.term_state
  -> key:term
  -> lemma:term
  -> AState.term_state

val get_lemma : p:AState.psi_def -> AState.term_state -> key:term -> term option

val add_lemmas_interactively
  :  p:AState.psi_def
  -> AState.refinement_loop_state
  -> AState.refinement_loop_state

val ith_synth_fun : int -> string

val synthfun_of_ctex
  :  p:AState.psi_def
  -> AState.term_state_detail
  -> int
  -> Syguslib.Sygus.command * (string * Syguslib.Sygus.sygus_sort) list * string

val term_var_string : term -> string

val convert_term_rec_to_ctex_rec
  :  p:AState.psi_def
  -> AState.term_state_detail
  -> AState.ctex
  -> string
  -> string

val ctex_model_to_args
  :  p:AState.psi_def
  -> AState.term_state_detail
  -> (string * Syguslib.Sygus.sygus_sort) list
  -> AState.ctex
  -> Syguslib.Sygus.sygus_term list

val constraint_of_neg_ctex
  :  int
  -> p:AState.psi_def
  -> AState.term_state_detail
  -> (string * Syguslib.Sygus.sygus_sort) list
  -> AState.ctex
  -> Syguslib.Sygus.command

val constraint_of_pos_ctex
  :  int
  -> p:AState.psi_def
  -> AState.term_state_detail
  -> (string * Syguslib.Sygus.sygus_sort) list
  -> AState.ctex
  -> Syguslib.Sygus.command

val log_soln : string -> variable list -> term -> unit

val handle_lemma_synth_response
  :  AState.term_state_detail
  -> Syguslib.Sygus.solver_response option Lwt.t * int Lwt.u
  -> (string * variable list * term) list option

val smt_of_recurs_elim_eqns : (term * term) list -> p:AState.psi_def -> SmtLib.smtTerm
val smt_of_aux_ensures : p:AState.psi_def -> SmtLib.smtTerm list
val smt_of_tinv_app : p:AState.psi_def -> AState.term_state_detail -> SmtLib.smtTerm
val smt_of_lemma_app : string * variable list * 'a -> SmtLib.smtTerm

val smt_of_lemma_validity
  :  p:AState.psi_def
  -> string * variable list * 'a
  -> AState.term_state_detail
  -> SmtLib.command list

val set_up_lemma_solver
  :  SmtInterface.AsyncSmt.solver
  -> p:AState.psi_def
  -> string * variable list * term
  -> unit Lwt.t

val synthesize_lemmas
  :  p:AState.psi_def
  -> Syguslib.Sygus.solver_response
     * ('a, Counterexamples.unrealizability_ctex list) Base.Either.t
  -> AState.refinement_loop_state
  -> (AState.refinement_loop_state, Syguslib.Sygus.solver_response) result
