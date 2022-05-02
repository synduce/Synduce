open TermTypes

module Variable : sig
  module T : sig
    type t = TermTypes.variable

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val ( = ) : t -> t -> bool
    val hash : 'a -> int
  end

  type t = variable

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val hash : 'a -> int

  type comparator_witness = Base.Comparator.Make(T).comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.t

  (** Assign a type to a variable. *)
  val vtype_assign : Context.t -> t -> RType.t -> unit

  (** Return the type of the variable, or [None] if it has no assigned types. *)
  val vtype : Context.t -> t -> RType.t option

  (** Return the id of the variable (function for [v.vid])  *)
  val id : t -> int

  (** Get the name of a variable identified by its integer id, and return [None] is there is no such
    variable. *)
  val get_name : Context.t -> int -> string option

  (** Forget the global type of the variable. *)
  val clear_type : Context.t -> t -> unit

  (** Return the type of a variable, or create a new type and return it if the variable doesn't have
      a type. That new type becomes the type of the variable.
    *)
  val vtype_or_new : Context.t -> t -> RType.t

  (** Apply a type substitution to all the knownn variables. *)
  val update_var_types : Context.t -> (RType.t * RType.t) list -> unit

  (** Create variable. [mk ~attrs ~t:(Some typ) name] creates a variable with name [name] ,type
      [typ] and attributes [attrs]. The variable is registered as well as its type if some type is
      provided, otherwise a type variable is generated.
    *)
  val mk
    :  ?attrs:(Attributes.elt, Attributes.Elt.comparator_witness) Base.Set.t
    -> ?t:RType.t option
    -> Context.t
    -> string
    -> t

  (** Returns true if the variable has the [Anonymous] attribute.  *)
  val is_anonymous : t -> bool

  (** Add the [Anonymous] attribute to the variable.  *)
  val make_anonymous : t -> t

  (** Returns true if the variable has the [Builtin] attribute.  *)
  val is_builtin : t -> bool

  (** Add the [Builtin] attribute to the variable. *)
  val make_builtin : t -> t

  (** Check whether the variable has an attribute.  *)
  val has_attr : Attributes.elt -> t -> bool

  (** [is_nonterminal v] returns true if [v] is a nonterminal in a PMRS. Inspects
        the attributes of the variable.
    *)
  val is_nonterminal : t -> bool

  (** [same_name v1 v2] eturns true if [v1] and [v2] have the same name. *)
  val same_name : t -> t -> bool

  val pp : Context.t -> Format.formatter -> t -> unit
  val pp_id : Format.formatter -> t -> unit
  val pp_typed : Context.t -> Format.formatter -> t -> unit
  val free : Context.t -> t -> unit
  val print_summary : Format.formatter -> Context.t -> unit
end

module VarSet : sig
  module V : sig
    type nonrec t = (variable, Variable.comparator_witness) Base.Set.t
  end

  type nonrec t =
    (variable, Base.Comparator.Make(Variable.T).comparator_witness) Base.Set.t

  type elt = variable

  val empty : (elt, Base.Comparator.Make(Variable.T).comparator_witness) Base.Set.t

  val singleton
    :  elt
    -> (elt, Base.Comparator.Make(Variable.T).comparator_witness) Base.Set.t

  val union_list
    :  (elt, Base.Comparator.Make(Variable.T).comparator_witness) Base.Set.t list
    -> (elt, Base.Comparator.Make(Variable.T).comparator_witness) Base.Set.t

  val elements : ('a, 'b) Base.Set.t -> 'a list

  val of_list
    :  elt list
    -> (elt, Base.Comparator.Make(Variable.T).comparator_witness) Base.Set.t

  val map : ('a -> elt) -> ('a, 'b) Base.Set.t -> V.t
  val filter_map : t -> f:(elt -> elt option) -> t

  (** [has_name vs n] returns true if there is a variable with name [n] in the set [vs] *)
  val has_name : t -> string -> bool

  (** Find a variable by its name.  *)
  val find_by_name : t -> string -> elt option

  (** Returns the list of ids of the variables in the set.  *)
  val vids_of_vs : t -> int list

  (** [has_vid vs i] returns true if there is a variable with id [id] in the set [vs] *)
  val has_vid : t -> int -> bool

  (** Find a variable by its id. *)
  val find_by_id : t -> int -> elt option

  (** Returns the association list of variable ids and variables.  *)
  val bindings : t -> (int * elt) list

  (** Return the list of names of the variables in the set. *)
  val names : t -> string list

  (** Filter a set with a type. *)
  val filter_by_type : Context.t -> t -> RType.t -> t

  (** Create a record type from a set of variables (an association list where keys are the variable
      names and data is the variable type). *)
  val record : Context.t -> t -> (string * RType.t) list

  (** Create an environment (a map from names to variables) from a variable set. *)
  val to_env : t -> (string, elt, Base.String.comparator_witness) Base.Map.t

  (** Given a set of variables, return a list of pairs such that for each element [v] in the set,
    there is a pair [v,v'] in the list where [v'] is a fresh variable (with a name similar to [v])
   *)
  val prime : Context.t -> t -> (elt * elt) list

  (** Change the names of the variables in the set by adding a string prefix. The variables are
      still equal to the input variable, in the sense that they have the same id, but different
      names.
  *)
  val add_prefix : t -> string -> t

  (** [iset vs ids] returns the set of variables in [vs] that have their id in [ids]. *)
  val iset : t -> int list -> t

  (** Pretty-print a set just by printing variable names.  *)
  val pp_var_names : Context.t -> Format.formatter -> (elt, 'a) Base.Set.t -> unit

  (** Pretty print a set by printing variable names with their type. Seee [pp_var_names] for
    alternative.  *)
  val pp : Context.t -> Format.formatter -> (elt, 'a) Base.Set.t -> unit

  (** Print the variable set in a non-human readable format.  *)
  val dump : Format.formatter -> (elt, 'a) Base.Set.t -> unit

  val of_sh
    :  ('a, elt) Base.Hashtbl.t
    -> (elt, Base.Comparator.Make(Variable.T).comparator_witness) Base.Set.t
end
