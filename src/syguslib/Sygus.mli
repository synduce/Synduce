(** This module contains definitions that mirror the SyGuS Language Standard Version 2.1,
    which can be found at https://sygus.org/assets/pdf/SyGuS-IF_2.1.pdf.
*)

(** {1 Types } *)

(** Set to true in order to use SyGuS language v1.*)
val use_v1 : bool ref

(** Symbols are just strings.  *)
type symbol = string

(** SyGuS literals. *)
type literal =
  | LitNum of int
      (** A numeral literal, which is either the digit 0 or a non-empty sequence of digits that
      does not being with 0. *)
  | LitDec of float
      (** A decimal number literal, which is syntactically
       <numeral>.0*<numeral>. *)
  | LitBool of bool (** A boolean literal is either true or false. *)
  | LitHex of symbol
      (** A hexadecimal is written with #x followed by a non empty sequence of
       characters [A-F] and [0-9] *)
  | LitBin of bool list
      (** A hexadecimal is written with #b followed by a non empty sequence of bits *)
  | LitString of symbol
      (** A string literal is any sequence of printable characters delimited by double
      quotes. *)

type index =
  | INum of int
  | ISym of symbol

(** An identifier can be simple, indexed or qualified.  *)
type identifier =
  | IdSimple of symbol (** A simple identifier is a symbol. *)
  | IdIndexed of symbol * index list
      (** An indexed identifier is (_ <symbol> <identifier>) *)
  | IdQual of symbol * sygus_sort (** A qualified identifier is (as <symbol> <sort>)*)

(** A sort  is either its id, or a sort applied to other sort (polymorphism) *)
and sygus_sort =
  | SId of identifier (** A simple sort with an identifier.  *)
  | SApp of identifier * sygus_sort list (** A sort applied to arguments.  *)

(** The type for sygus terms. *)
type sygus_term =
  | SyId of identifier (*** An identifier. *)
  | SyLit of literal (** A literal. *)
  | SyApp of identifier * sygus_term list (** A function application. *)
  | SyExists of sorted_var list * sygus_term (** An existential quantifier. *)
  | SyForall of sorted_var list * sygus_term (** A universal quantifier. *)
  | SyLet of binding list * sygus_term (** A let-binding. *)

and sorted_var = symbol * sygus_sort

and binding = symbol * sygus_term

type feature =
  | FGrammar
  | FFwdDecls
  | FRecursion

(** The SyGuS commands  *)
type command =
  | CCheckSynth (** The [(check-synth)] command for synthesis. *)
  | CConstraint of sygus_term
      (** The constraint command to define a constraint with a term. *)
  | CDeclareVar of symbol * sygus_sort
      (** The declare command to declare a universally quantified variable. *)
  | CInvConstraint of symbol * symbol * symbol * symbol
      (** The inv-constraint command to constraint an invariant. *)
  | CSetFeature of feature * bool (** The set-feature command. *)
  | CSynthFun of symbol * sorted_var list * sygus_sort * grammar_def option
      (** The synth-fun command to declare a function to synthesize. The grammar is optional. *)
  | CSynthInv of symbol * sorted_var list * grammar_def option
      (** The synth-inv command is a shortcut for synth-fun when the function to be synthesized
        returns a boolean. It is printed as synth-fun ... Bool if use_v1 is false. *)
  | CDeclareDataType of symbol * dt_cons_dec list
      (** A datatype declaration with a name and constructor list. *)
  | CDeclareDataTypes of sygus_sort_decl list * dt_cons_dec list list
      (** A declaration for mutually recursive data types. *)
  | CDeclareSort of symbol * int (** A sort declaration *)
  | CDefineFun of symbol * sorted_var list * sygus_sort * sygus_term
      (** A function definition. *)
  | CDefineSort of symbol * sygus_sort (** A sort definition (renaming). *)
  | CSetInfo of symbol * literal (** Setting some solver option.  *)
  | CSetLogic of symbol (** Setting the logic used.  *)
  | CSetOption of symbol * literal (** Setting some solver option.  *)

(** A sort declaration is a name with an integer index.  *)
and sygus_sort_decl = symbol * int

(** A datatype constructor with a name and named arguments.  *)
and dt_cons_dec = symbol * sorted_var list

(** A grammar definition is a list of non-terminals together with their production rule.
    In SyGuS v2, the definition is printed by first declaring the non-terminals and then
    printing the rules. The first symbol is always assumed to be the start symbol.
 *)
and grammar_def = (sorted_var * grouped_rule_list) list

and grouped_rule_list = sygus_gsterm list

(** A gramamr term in SyGuS.  *)
and sygus_gsterm =
  | GConstant of sygus_sort (** A constant stands for any constant of a given sort.  *)
  | GTerm of sygus_term
      (** A grammar term stands for a SyGuS term, which may contain  non-terminals
      of the grammar. *)
  | GVar of sygus_sort (** A grammar var stands for any variable of a given sort. *)

(** A program in SyGuS is a list of commands. *)
type program = command list

(** The special characters that may not be used as identifiers (symbols). *)
val special_chars : char list

(** A list of reserved symbols, that may not be used as symbols. *)
val reserved_words : symbol list

val digits : char list

(** Returns true if the string is a valid identifier in SyGuS.  *)
val valid_ident : symbol -> bool

(** {1 Conversion to s-expressions } *)

(** *)
val sexp_of_symbol : symbol -> Sexplib0.Sexp.t

val sexp_of_index : index -> Sexplib0.Sexp.t
val sexp_of_identifier : identifier -> Sexplib0.Sexp.t
val sexp_of_sygus_sort : sygus_sort -> Sexplib0.Sexp.t
val bool_list_to_bin_string : bool list -> symbol
val sexp_of_literal : literal -> Sexplib0.Sexp.t
val sexp_of_sorted_var : sorted_var -> Sexplib0.Sexp.t
val sexp_of_sygus_term : sygus_term -> Sexplib0.Sexp.t
val sexp_of_feature : feature -> Sexplib0.Sexp.t
val sexp_of_sort_decl : sygus_sort_decl -> Sexplib0.Sexp.t
val sexp_of_dt_cons_dec : dt_cons_dec -> Sexplib0.Sexp.t
val sexp_of_sygus_gsterm : sygus_gsterm -> Sexplib0.Sexp.t
val sexp_of_grammar_def : grammar_def -> Sexplib0.Sexp.t * Sexplib0.Sexp.t
val sexp_of_command : command -> Sexplib0.Sexp.t
val sexp_list_of_program : program -> Sexplib0.Sexp.t list

(** {1 Conversion from s-expressions} *)

(** *)
val char_to_bool : char -> bool

val literal_of_string : symbol -> literal
val literal_of_sexp : Sexplib0.Sexp.t -> literal
val feature_of_sexp : Sexplib0.Sexp.t -> feature
val sorted_var_of_sexp : Sexplib0.Sexp.t -> sorted_var
val sygus_term_of_sexp : Sexplib0.Sexp.t -> sygus_term
val symbol_of_sexp : Sexplib0.Sexp.t -> symbol
val index_of_sexp : Sexplib0.Sexp.t -> index
val identifier_of_sexp : Sexplib0.Sexp.t -> identifier
val sygus_sort_decl_of_sexp : Sexplib0.Sexp.t -> sygus_sort_decl
val sygus_sort_of_sexp : Sexplib0.Sexp.t -> sygus_sort
val sygus_sort_decl_list_of_sexp : Sexplib0.Sexp.t -> sygus_sort_decl list
val binding_of_sexp : Sexplib0.Sexp.t -> binding
val dt_cons_dec_of_sexp : Sexplib0.Sexp.t -> dt_cons_dec
val dt_cons_dec_list_of_sexp : Sexplib0.Sexp.t -> dt_cons_dec list
val sygus_gsterm_of_sexp : Sexplib0.Sexp.t -> sygus_gsterm
val pre_grouped_rule_of_sexp : Sexplib0.Sexp.t -> symbol * sygus_sort * sygus_gsterm list
val grammar_def_of_sexps : Sexplib0.Sexp.t -> Sexplib0.Sexp.t -> grammar_def
val command_of_sexp : Sexplib0.Sexp.t -> command
val program_of_sexp_list : Sexplib0.Sexp.t list -> program

(** {1 Solver specific types.} *)

(** The types of responses a solver can give.  *)
type solver_response =
  | RSuccess of (symbol * sorted_var list * sygus_sort * sygus_term) list
      (** A successful response comes with a list of implementations for the functions to
      synthesize. *)
  | RInfeasible (** A response indicating that the problem has no solution. *)
  | RFail (** A response indicating that the solver failed. *)
  | RUnknown
      (** A response indicating that the solver could not find a solution, with
        no guarantee about the feasibility of the problem. *)

(** Returns true is the solver response is successful.  *)
val is_sat : solver_response -> bool

(** Returns true if the solver response indicates failure. *)
val is_failed : solver_response -> bool

(** Returns true if the solver response indicates the problem is is infeasible *)
val is_infeasible : solver_response -> bool

(** Translate a s-expression to a solver response. *)
val reponse_of_sexps : Sexplib0.Sexp.t list -> solver_response

module SyCommand : sig
  type t = command

  val of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of : t -> Sexplib0.Sexp.t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit
end

module SyTerm : sig
  type t = sygus_term

  val of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of : t -> Sexplib0.Sexp.t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit
end

module SyIdentifier : sig
  type t = identifier

  val of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of : t -> Sexplib0.Sexp.t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit
end

module SyLiteral : sig
  type t = literal

  val of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of : t -> Sexplib0.Sexp.t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit
end

module SySort : sig
  type t = sygus_sort

  val of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of : t -> Sexplib0.Sexp.t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit
end

(** Write a sygus command to an out_channel.  *)
val write_command : out_channel -> command -> unit

(** Returns true if the command sets an options or a logic.  *)
val is_setter_command : command -> bool

(** Returns true if the program passes some well-formedness checks, according to the following rules:
   {ul
     {li A SyGuS input is not well-formed if it specifies a list of commands that do not meet the
     restrictions given in this section regarding their order. The order is specified by the
     following regular pattern: [(set logic command)? (setter commands)∗(other sygus_commands)∗]}
     }
*)
val is_well_formed : program -> bool

(** Returns the list of symbols that are declared by the command. *)
val declares : command -> symbol list

(** Compare two commands by the symbols they declare. *)
val compare_declares : command -> command -> int

(** Static definition for the max operation: (max x y) is (ite (>= x y) x y))  *)
val max_definition : command

(** Static definition for the min operation: (min x y) is (ite (<= x y) x y))  *)
val min_definition : command

(** Apply a substitution to the symbols in a sygus term.  *)
val rename : (symbol * symbol) list -> sygus_term -> sygus_term
