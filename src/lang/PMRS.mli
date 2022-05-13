open Base
open Term
open Utils

(**
  A PMRS rewrite rule [(v, args, pattern, rhs)] is the rewrite rule
  v(args,pattern) -> rhs.
  [v] which is a non-terminal (a recursive function in the OCaml code),
  [args] is a possibly empty list of arguments,
  [pattern] is an optional pattern,
  [rhs] is the production of the rule.
*)
type rewrite_rule = variable * variable list * pattern option * term

(**
  A toplevel function is a triple of a variable (the variable representing
  the function itself), a list of variables (the arguments of the function) and
  a term (the body of the function).
*)
type top_function = variable * variable list * term

(** The main PMRS type.  *)
type t =
  { pvar : Variable.t
        (**
    The main function symbol, storing general information about the PMRS.
    The specification associated to the PMRS may be found using:
    [get_spec pvar]
    The type of the PMRS (the main function of the group of mutually recursive
    functions) is stored as the type of pvar.
  *)
  ; pinput_typ : RType.t list
        (**
    The input type(s). For now, it should be a singleton list.
    The input type is only the type of the recursively-typed argument of the PMRS, not the parameters.
  *)
  ; poutput_typ : RType.t (** Output type and optional invariant on output of function. *)
  ; pargs : Variable.t list (** Parameter arguments.  *)
  ; psyntobjs : Variables.VarSet.t (** The unknowns to synthesize.*)
  ; prules : rewrite_rule IntMap.t (** The rules of the PMRS. *)
  ; pnon_terminals : VarSet.t (** Non-terminals of the PMRS. *)
  ; pmain_symb : variable (** The main symbol of the PMRS. *)
  ; porder : int (** The order of the PMRS (mostly useless for now). *)
  ; plogic : SmtLogic.logic_info
  }

(** Context for:
    - all PMRS in the file, indexed by the function variable id.
    - all the nonterminals, indexed by the function variable id.
*)
module Functions : sig
  type ctx =
    { globals : (int, t) Hashtbl.t
    ; nonterminals : (int, int) Hashtbl.t
    }

  (** Create an empty context, with an empty table of functions. *)
  val create : unit -> ctx

  (** Clear the function tables. *)
  val clear : ctx -> unit

  (** Copy a function context (copies the tables)*)
  val copy : ctx -> ctx

  (** Add a global function to the table of functions [globals] inside the context. *)
  val register_global : ctx -> t -> unit

  (** Search for a PMRS with the given id in the global table of functions. *)
  val find_global : ctx -> int -> t option

  (** Find a PMRS by name. The name of a PMRS is the name of its pvar.  *)
  val find_by_name : ctx -> string -> variable option

  (** Register a non-terminal symbol within a PRMS *)
  val register_nonterminal : ctx -> int -> t -> unit

  (** Find a non-terminal symbol in the current context (returns the PMRS that defines it) *)
  val find_nonterminal : ctx -> int -> t option

  (** Find a non-terminal (represented by a variable) from its name. *)
  val find_nonterminal_by_name : ctx -> string -> variable option

  (** [update ctx p] registers the PMRS [p] and all its non-terminals inside the context [ctx].*)
  val update : ctx -> t -> unit
end

(** Update the order of a PMRS.  *)
val update_order : t -> t

(** Set the logic info field of a PMRS.  *)
val set_logic_info_of_pmrs : ctx:Context.t -> t -> t

(** {-1 Utility functions, transformation, conversion to functions.} *)

(** Translate a recursive function to a PMRS (currently only work for identity).  *)
val func_to_pmrs : ctx:Context.t -> Variable.t -> Term.fpattern list -> Term.term -> t

(** Translate a PMRS to a set of (mutually) recursive functions. *)
val func_of_pmrs : ctx:Context.t -> t -> function_descr list

(**
  inverted_rule_lookup searches for rules whose rhs match (func args), and return
  a map from rule id to the lhs of the rules matching (func args), with the appropriate
  substitutions performed.
*)
val inverted_rule_lookup
  :  ?boundvars:(Variable.t, Variable.comparator_witness) Base.Set.t
  -> ctx:Context.t
  -> ('a, Variable.t * Variable.t list * Term.pattern option * Term.term, 'b) Base.Map.t
  -> Term.term
  -> Term.term list
  -> ('a, Term.term, 'b) Base.Map.t

(**
  Apply a substitution to all the right hand side of the PMRS rules.
*)
val subst_rule_rhs : ctx:Context.t -> p:t -> (Term.term * Term.term) list -> t

(**
  Given an input PMRS, returns a list of PMRS that this PMRS depends on.
  *)
val depends : glob:Functions.ctx -> ctx:Context.t -> t -> t list

(**
  Generate the term that corresponds to the left hand side of a rewrite rule in
  a PMRS.
*)
val lhs : ctx:Context.t -> variable * variable list * pattern option * term -> term

(**
  Updates and returns the output type of a PMRS.
*)
val update_output_type : ctx:Context.t -> t -> t

(**{-1 PMRS and Types }*)

(** Clear the type information inside a PMRS.  *)
val clear_pmrs_types : ctx:Context.t -> t -> t

(** Infer the types inside a PMRS. *)
val infer_pmrs_types : ctx:Context.t -> t -> t

val unify_two_with_vartype_update
  :  ctx:Context.t
  -> RType.t * RType.t
  -> RType.t * RType.t
  -> RType.substitution

val unify_one_with_update : ctx:Context.t -> RType.t * RType.t -> unit
val extract_rec_input_typ : t -> RType.t

(**{-1 Pretty printing functions.}*)

(** Pretty print a rewrite rule, as in a PMRS. *)
val pp_rewrite_rule : ctx:Context.t -> Formatter.t -> rewrite_rule -> unit

(** Pretty print a PMRS. *)
val pp : ctx:Context.t -> Formatter.t -> ?short:bool -> t -> unit

(** Pretty print a PMRS using Ocaml syntax (as a set of recursive functions). *)
val pp_ocaml : ctx:Context.t -> Formatter.t -> ?short:bool -> t -> unit
