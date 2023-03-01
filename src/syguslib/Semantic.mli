(** This module defines a few high-level properties of SyGuS programs.
  Setter commands and some utility functions on solver responses are specified here.
*)

open Sygus
open Base

(** {1 Semantic identity of commands} *)

(** Returns true when the command is a setter command, that is, sets
  some option. logic, or feature for the solver.
*)
val is_setter_command : Sygus.command -> bool

(** Returns true if the program is well-formed.
    A well-formed program has the form
    (set-logic ..)(setter-command ..)*(other-commands..)*
*)
val is_well_formed : Sygus.program -> bool

(** Returns the list of symbols declared by a command, in no particular order  *)
val declares : Sygus.command -> string list

(** Compare two commands by the symbols they declare.  *)
val compare_declares : Sygus.command -> Sygus.command -> int

(** {1 Static definitions} *)

(** A static definition for the max operation.  *)
val max_definition : Sygus.command

(** A static definition for the min operation.  *)
val min_definition : Sygus.command

(** {1 Transformers} *)

(** A transformation function that renames all variables in a sygus term.  *)
val rename : (string * string) list -> Sygus.sygus_term -> Sygus.sygus_term

(** {1 I/O functions} *)

(** Writes a command to an output channel.  *)
val write_command : Stdio.Out_channel.t -> Sygus.command -> unit

(** {1 Wrapper modules for important types} *)

module Command : sig
  type t = command

  val of_sexp : Sexp.t -> t
  val sexp_of : t -> Sexp.t
  val pp : Formatter.t -> t -> unit
  val pp_hum : Formatter.t -> t -> unit
end

module Term : sig
  type t = sygus_term

  val of_sexp : Sexp.t -> t
  val sexp_of : t -> Sexp.t
  val pp : Formatter.t -> t -> unit
  val pp_hum : Formatter.t -> t -> unit
end

module Ident : sig
  type t = identifier

  val of_sexp : Sexp.t -> t
  val sexp_of : t -> Sexp.t
  val pp : Formatter.t -> t -> unit
  val pp_hum : Formatter.t -> t -> unit
end

module Lit : sig
  type t = literal

  val of_sexp : Sexp.t -> t
  val sexp_of : t -> Sexp.t
  val pp : Formatter.t -> t -> unit
  val pp_hum : Formatter.t -> t -> unit
end

module Sort : sig
  type t = sygus_sort

  val of_sexp : Sexp.t -> t
  val sexp_of : t -> Sexp.t
  val pp : Formatter.t -> t -> unit
  val pp_hum : Formatter.t -> t -> unit
end
