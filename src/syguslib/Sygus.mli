(** This module contains definitions that mirror the SyGuS Language Standard Version 2.1,
    which can be found at https://org/assets/pdf/SyGuS-IF_2.1.pdf.
*)

type location = Parsexp.Positions.range
type position = Parsexp.Positions.pos

val dummy_loc : location
val pp_pos : Format.formatter -> position -> unit
val pp_loc : Format.formatter -> location -> unit

(** {1 Types } *)

(** Set to true in order to use SyGuS language v1.*)
val use_v1 : bool ref

(** Symbols are just strings.  *)
type symbol = string

type attribute =
  | Attr of location * symbol
  | AttrVal of location * symbol * string

(** SyGuS literals. *)
type literal =
  | LitNum of location * int
      (** A numeral literal, which is either the digit 0 or a non-empty sequence of digits that
      does not begin with 0. *)
  | LitDec of location * float
      (** A decimal number literal, which is syntactically
       <numeral>.0*<numeral>. *)
  | LitBool of location * bool (** A boolean literal is either true or false. *)
  | LitHex of location * symbol
      (** A hexadecimal is written with #x followed by a non empty sequence of
       characters [A-F] and [0-9] *)
  | LitBin of location * bool list
      (** A hexadecimal is written with #b followed by a non empty sequence of bits *)
  | LitString of location * symbol
      (** A string literal is any sequence of printable characters delimited by double
      quotes. *)

type index =
  | INum of location * int
  | ISym of location * symbol

(** An identifier can be simple, indexed or qualified.  *)
type identifier =
  | IdSimple of location * symbol (** A simple identifier is a symbol. *)
  | IdIndexed of location * symbol * index list
      (** An indexed identifier is (_ <symbol> <identifier>) *)
  | IdQual of location * symbol * sygus_sort
      (** A qualified identifier is (as <symbol> <sort>)*)

(** A sort  is either its id, or a sort applied to other sort (polymorphism) *)
and sygus_sort =
  | SId of location * identifier (** A simple sort with an identifier.  *)
  | SApp of location * identifier * sygus_sort list (** A sort applied to arguments.  *)

(** The type for sygus terms. *)
type sygus_term =
  | SyId of location * identifier (*** An identifier. *)
  | SyLit of location * literal (** A literal. *)
  | SyApp of location * identifier * sygus_term list (** A function application. *)
  | SyExists of location * sorted_var list * sygus_term (** An existential quantifier. *)
  | SyForall of location * sorted_var list * sygus_term (** A universal quantifier. *)
  | SyLet of location * binding list * sygus_term (** A let-binding. *)

and sorted_var = location * symbol * sygus_sort
and binding = location * symbol * sygus_term

type feature =
  | FGrammar
  | FFwdDecls
  | FRecursion
  | FOracles
  | FWeights

(** The SyGuS commands  *)
type command =
  | CCheckSynth of location (** The [(check-synth)] command for synthesis. *)
  | CAssume of location * sygus_term (** The assume command. *)
  | CConstraint of location * sygus_term
      (** The constraint command to define a constraint with a term. *)
  | CChcConstraint of location * sorted_var list * sygus_term * sygus_term
      (** A command for specifying CHC constraints. *)
  | CDeclareVar of location * symbol * sygus_sort
      (** The declare command to declare a universally quantified variable. *)
  | CDeclareWeight of location * symbol * attribute list
      (** Declaration of weight attributes. *)
  | CInvConstraint of location * symbol * symbol * symbol * symbol
      (** The inv-constraint command to constraint an invariant. *)
  | CSetFeature of location * feature * bool (** The set-feature command. *)
  | CSynthFun of location * symbol * sorted_var list * sygus_sort * grammar_def option
      (** The synth-fun command to declare a function to synthesize. The grammar is optional. *)
  | CSynthInv of location * symbol * sorted_var list * grammar_def option
      (** The synth-inv command is a shortcut for synth-fun when the function to be synthesized
        returns a boolean. It is printed as synth-fun ... Bool if use_v1 is false. *)
  | COptimizeSynth of location * sygus_term list * attribute list
      (** Command for specifying optimization queries. *)
  | CDeclareDataType of location * symbol * dt_cons_dec list
      (** A datatype declaration with a name and constructor list. *)
  | CDeclareDataTypes of location * sygus_sort_decl list * dt_cons_dec list list
      (** A declaration for mutually recursive data types. *)
  | CDeclareSort of location * symbol * int (** A sort declaration *)
  | CDefineFun of location * symbol * sorted_var list * sygus_sort * sygus_term
      (** A function definition. *)
  | CDefineSort of location * symbol * sygus_sort (** A sort definition (renaming). *)
  | CSetInfo of location * symbol * literal (** Setting some solver option.  *)
  | CSetLogic of location * string (** Setting the logic used.  *)
  | CSetOption of location * string * literal (** Setting some solver option.  *)
  | COracle of location * oracle_command
      (** All the oracle commands are grouped under COracle. *)

(** A different type is used to separate the oracle commands. *)
and oracle_command =
  | OAssume of sorted_var list * sorted_var list * sygus_term * symbol
      (** Asert an oracle assumption. *)
  | OConstraint of sorted_var list * sorted_var list * sygus_term * symbol
      (** Assert and oracle constraint. *)
  | ODeclareFun of symbol * sygus_sort list * sygus_sort * symbol
      (** Declare an oracle functional symbol. *)
  | OConstraintIO of symbol * symbol (** Declare an input-output oracle. *)
  | OConstraintCex of symbol * symbol (** Declare a counterexample witness oracle. *)
  | OConstraintMem of symbol * symbol (** Declare a membership-query oracle. *)
  | OConstraintPosw of symbol * symbol (** Declare a positive witness oracle. *)
  | OConstraintNegw of symbol * symbol (** Declare a negative witness oracle.  *)
  | OCorrectness of symbol * symbol (** Declare a correctness oracle.  *)
  | OCorrectnessCex of symbol * symbol
      (** Declare a correctness oracle with counterexamples. *)

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

(** A grammar term in   *)
and sygus_gsterm =
  | GConstant of location * sygus_sort
      (** A constant stands for any constant of a given sort.  *)
  | GTerm of location * sygus_term
      (** A grammar term stands for a SyGuS term, which may contain  non-terminals
      of the grammar. *)
  | GVar of location * sygus_sort
      (** A grammar var stands for any variable of a given sort. *)

(** A program in SyGuS is a list of commands. *)
type program = command list

(** The special characters that may not be used as identifiers (symbols). *)
val special_chars : char list

(** A list of reserved symbols, that may not be used as symbols. *)
val reserved_words : symbol list

val digits : char list

(** Returns true if the string is a valid identifier in   *)
val valid_ident : symbol -> bool

(** Checks that a filename has the standard extension (.sl) *)
val has_standard_extension : string -> bool

val char_to_bool : char -> bool

(** {1 Building terms.} *)

(** Build a symbol. Right now, the function is just identity. *)
val mk_symbol : string -> symbol

(** Build a simple attribute with no value.  *)
val mk_attr : ?loc:location -> string -> attribute

(** Build an attribute with some value.  *)
val mk_attr_val : ?loc:location -> string -> string -> attribute

(** Build a numeral literal.  *)
val mk_lit_num : ?loc:location -> int -> literal

(** Build a decimal literal. *)
val mk_lit_dec : ?loc:location -> float -> literal

(** Build a boolean literal. *)
val mk_lit_bool : ?loc:location -> bool -> literal

(** Build a hex literal. *)
val mk_lit_hex : ?loc:location -> string -> literal

(** Build a binary literal. *)
val mk_lit_bin : ?loc:location -> bool list -> literal

(** Build a string literal. *)
val mk_lit_string : ?loc:location -> string -> literal

(** Build a term containing a numeral literal.  *)
val mk_num : ?loc:location -> int -> sygus_term

(** Build a term containing a decimal literal.  *)
val mk_dec : ?loc:location -> float -> sygus_term

(** Build a term containing a boolean literal.  *)
val mk_bool : ?loc:location -> bool -> sygus_term

(** Build a term containing a hexadecimal literal.  *)
val mk_hex : ?loc:location -> string -> sygus_term

(** Build a term containing a binary literal.  *)
val mk_bin : ?loc:location -> bool list -> sygus_term

(** Build a term containing a string literal.  *)
val mk_string : ?loc:location -> string -> sygus_term

(** Build a concrete numeral index.  *)
val mk_index_num : ?loc:location -> int -> index

(** Build a symbolic index.  *)
val mk_index_sym : ?loc:location -> string -> index

(** Build a simple identifier with a string.  *)
val mk_id_simple : ?loc:location -> string -> identifier

(** Build an indexed identifier.  *)
val mk_id_indexed : ?loc:location -> string -> index list -> identifier

(** Build a quailified identifier.  *)
val mk_id_qual : ?loc:location -> string -> sygus_sort -> identifier

(** Build a sort from a sort name.  *)
val mk_sort : ?loc:location -> identifier -> sygus_sort

(** Build a parametric sort.  *)
val mk_sort_app : ?loc:location -> identifier -> sygus_sort list -> sygus_sort

(** Build a term from an identifier. *)
val mk_t_id : ?loc:location -> identifier -> sygus_term

(** Build a term that contains a simple id.  *)
val mk_simple_id : ?loc:location -> symbol -> sygus_term

(** Build a term from a literal.  *)
val mk_t_lit : ?loc:location -> literal -> sygus_term

(** Build a term application.  *)
val mk_t_app : ?loc:location -> identifier -> sygus_term list -> sygus_term

(** Build a (exists (...) ...) term *)
val mk_t_exists : ?loc:location -> sorted_var list -> sygus_term -> sygus_term

(** Build a (forall (...) ...) term *)
val mk_t_forall : ?loc:location -> sorted_var list -> sygus_term -> sygus_term

(** Build a let binding term.  *)
val mk_t_let : ?loc:location -> binding list -> sygus_term -> sygus_term

val mk_binding : ?loc:location -> string -> sygus_term -> binding
val mk_sorted_var : ?loc:location -> string -> sygus_sort -> sorted_var

(** Generate the (check-synth) command  *)
val mk_c_check_synth : ?loc:location -> unit -> command

(** [mk_c_assume ~loc t] builds the [(assume t)] command at location [loc]  *)
val mk_c_assume : ?loc:location -> sygus_term -> command

(** [mk_c_constraint ~loc t] builds the [(constraint t)] command at location [loc]  *)
val mk_c_constraint : ?loc:location -> sygus_term -> command

val mk_c_chc_constraint
  :  ?loc:location
  -> sorted_var list
  -> sygus_term
  -> sygus_term
  -> command

val mk_c_declare_var : ?loc:location -> string -> sygus_sort -> command
val mk_c_declare_weight : ?loc:location -> string -> attribute list -> command
val mk_c_inv_constraint : ?loc:location -> string -> string -> string -> string -> command
val mk_c_set_feature : ?loc:location -> feature -> bool -> command

val mk_c_synth_fun
  :  ?loc:location
  -> ?g:grammar_def option
  -> string
  -> sorted_var list
  -> sygus_sort
  -> command

val mk_c_synth_inv
  :  ?loc:location
  -> ?g:grammar_def option
  -> string
  -> sorted_var list
  -> command

val mk_c_optimize_synth : ?loc:location -> sygus_term list -> attribute list -> command
val mk_c_declare_datatype : ?loc:location -> string -> dt_cons_dec list -> command

val mk_c_declare_datatypes
  :  ?loc:location
  -> sygus_sort_decl list
  -> dt_cons_dec list list
  -> command

val mk_c_declare_sort : ?loc:location -> string -> int -> command

val mk_c_define_fun
  :  ?loc:location
  -> string
  -> sorted_var list
  -> sygus_sort
  -> sygus_term
  -> command

val mk_c_define_sort : ?loc:location -> string -> sygus_sort -> command
val mk_c_set_info : ?loc:location -> string -> literal -> command
val mk_c_set_logic : ?loc:location -> string -> command
val mk_c_set_option : ?loc:location -> string -> literal -> command
val mk_c_oracle : ?loc:location -> oracle_command -> command
val mk_g_constant : ?loc:location -> sygus_sort -> sygus_gsterm
val mk_g_term : ?loc:location -> sygus_term -> sygus_gsterm
val mk_g_var : ?loc:location -> sygus_sort -> sygus_gsterm

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
