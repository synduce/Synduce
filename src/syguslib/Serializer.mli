(** A Sygus command or a sygus term is an s-expression. This modules defines functions
    to convert from a SyGuS term to an s-expression. The functions in this module should
    be self-explanatory.
*)

val sexp_of_symbol : string -> Sexplib0.Sexp.t
val keyword_of_string : string -> string
val sexp_of_attribute : Sygus.attribute -> Sexplib0.Sexp.t list
val sexp_of_index : Sygus.index -> Sexplib0.Sexp.t
val sexp_of_identifier : Sygus.identifier -> Sexplib0.Sexp.t
val sexp_of_sygus_sort : Sygus.sygus_sort -> Sexplib0.Sexp.t
val bool_list_to_bin_string : bool list -> string
val sexp_of_literal : Sygus.literal -> Sexplib0.Sexp.t
val sexp_of_sorted_var : Sygus.sorted_var -> Sexplib0.Sexp.t
val sexp_of_sygus_term : Sygus.sygus_term -> Sexplib0.Sexp.t
val sexp_of_feature : Sygus.feature -> Sexplib0.Sexp.t
val sexp_of_sort_decl : Sygus.sygus_sort_decl -> Sexplib0.Sexp.t
val sexp_of_dt_cons_dec : Sygus.dt_cons_dec -> Sexplib0.Sexp.t
val sexp_of_sygus_gsterm : Sygus.sygus_gsterm -> Sexplib0.Sexp.t
val sexp_of_grammar_def : Sygus.grammar_def -> Sexplib0.Sexp.t * Sexplib0.Sexp.t
val sexp_of_oracle_command : Sygus.oracle_command -> Sexplib0.Sexp.t
val sexp_of_command : Sygus.command -> Sexplib0.Sexp.t
val sexp_list_of_program : Sygus.program -> Sexplib0.Sexp.t list
