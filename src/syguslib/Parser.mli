(** This module defines function to convert from s-expressions to sygus.
*)

open Base
open Sygus

module Annot : sig
  type kind =
    | AAtom of string
    | AList of t list

  and t =
    { loc : Parsexp.Positions.range
    ; sexp : kind
    }

  (** Add dummy locations to a s-expression.  *)
  val of_sexp : Sexp.t -> t

  val iter : t -> f:(location -> Sexp.t -> unit) -> unit
end

(** An exception raised during the s-expression based parsing method.
  The s-expression should be the expression causing the exception to be raise,
  and the string is an informative message as to why parsing failed.
*)
exception ParseError of location * Sexp.t * string

(** An exception raise during parsing when an input does not conform to the new
  v2.1 standard (e.g. and input of v1).
  This exception contains the command resulting from parsing the input.
*)
exception NonConforming of command * string

(** {1 Main entry points}  *)

(** Parse a file using the s-epxression based parser.
  @raise ParseError
*)
val sexp_parse : string -> program

(** Convert a s-expression into a command {!type:Sygus.command}.
  Raises {!exception:ParseError} if the s-expression is not a command.
  Raises {!exception:NonConforming} is the s-expression is not a SyGuS
  v2 command, byt may be a SyGuS v1 command.
  @raise ParseError
  @raise NonConforming
  *)
val command_of_asexp : Annot.t -> command

(** Convert a list of s-expressions into a prgram, which is a list of
    commands.
    Raises {!exception:ParseError} if parsing one of the commands fails.
    @raise ParseError
    @raise NonConforming
    *)
val program_of_asexp_list : Annot.t list -> program

(**
      Translate a s-expression returned by a solver to a {!type:Sygus.solver_response}.
  *)
val response_of_asexps : Annot.t list -> solver_response

(** {1 Useful auxiliary functions.}  *)

(** Convert a s-expression into a symbol.
  Raises {!exception:ParseError} if the s-expression is not a atom.
  @raise ParseError
  *)
val symbol_of_asexp : Annot.t -> string

(** Convert a s-expression into a feature {!type:Sygus.feature}.
  Raises {!exception:ParseError} if the s-expression is not one of
  the predefined features.
  @raise ParseError
  *)
val feature_of_asexp : Annot.t -> (feature, location * string) Result.t

(** Convert a s-expression into an index. {!type:Sygus.index}.
  Raises {!exception:ParseError} if the s-expression is not an index.
  @raise ParseError
  *)
val index_of_asexp : Annot.t -> index

(** Convert a s-expression into an identifier {!type:Sygus.identifier}.
  Raises {!exception:ParseError} if the s-expression is not an identifier
  (either an indexed identifier or a simple identifier).
  @raise ParseError
  *)
val identifier_of_asexp : Annot.t -> identifier

(** Convert a s-expression into a sort {!type:Sygus.sygus_sort}.
  Raises {!exception:ParseError} if the s-expression is not a valid sort.
  @raise ParseError
  *)
val sygus_sort_of_asexp : Annot.t -> sygus_sort

(** Convert a s-expression into a sorted var {!type:Sygus.sorted_var}.
  Raises {!exception:ParseError} if the s-expression is not a sorted var,
  which is a pair of a symbol and a sort.
  @raise ParseError
  *)
val sorted_var_of_asexp : Annot.t -> sorted_var

(** Convert a string into a literal {!type:Sygus.literal}.
  Raises {!exception:ParseError} if the s-expression is not a literal.
  @raise ParseError
  *)
val literal_of_string : ?loc:location -> string -> literal

(** Convert a s-expression into a literal {!type:Sygus.feature}.
  Raises {!exception:ParseError} if the s-expression is not an atom,
  and if the string in the atom is not a literal.
  @raise ParseError
  *)
val literal_of_asexp : Annot.t -> literal

(** Convert a s-expression into a term {!type:Sygus.sygus_term}.
  Raises {!exception:ParseError} if the s-expression is not a term.
  @raise ParseError
  *)
val sygus_term_of_asexp : Annot.t -> sygus_term

(** Convert a s-expression into a binding {!type:Sygus.binding}.
  Raises {!exception:ParseError} if the s-expression is not a pair of
  and identifier and a term (a binding).
  @raise ParseError
  *)
val binding_of_asexp : Annot.t -> binding

(** Convert a s-expression into a sort declaration {!type:Sygus.sygus_sort_decl}.
  @raise ParseError
  *)
val sygus_sort_decl_of_asexp : Annot.t -> sygus_sort_decl

(** Convert a s-expression into a datatype constructor declaration
 {!type:Sygus.dt_cons_dec}.
  @raise ParseError
  *)
val dt_cons_dec_of_asexp : Annot.t -> dt_cons_dec

(** Convert a s-expression into a grammar term {!type:Sygus.sygus_gsterm}.
  @raise ParseError
  *)
val sygus_gsterm_of_asexp : Annot.t -> sygus_gsterm

(** Convert a s-expression into a grammar description.
  @raise ParseError
  *)
val pre_grouped_rule_of_asexp
  :  Annot.t
  -> location * string * sygus_sort * sygus_gsterm list

(** Convert a s-expression into a grammar definition.
  @raise ParseError
  *)
val grammar_def_of_asexps : Annot.t option -> Annot.t -> grammar_def
