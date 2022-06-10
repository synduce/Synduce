open Lang
open TermTypes

(** A configuration is a map from variables (the unknowns) to lists
    of terms (the arguments of the unknown).
*)
type conf = term list Term.VarMap.t

(** Pretty-printer for configurations.  *)
val ppm : Env.env -> Format.formatter -> conf -> unit

(** A module for subconfigurations with some utility functions and the functions
    necessary to constructs sets and graphs of subconfigurations.
*)
module Subconf : sig
  (** A subconfigurations is a map from integers (the unknown ids) to
        list of integers (the position of the argument in a sup-configuration)
    *)
  type t = int list Utils.IntMap.t

  val to_string : t -> string
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  (** Create the maximum sub-configuration: all arguments of the sup-conf are taken. *)
  val of_conf : conf -> t

  (** Create the minimum sub-configuration: a map where all argument lists are empty. *)
  val zero_of_conf : conf -> t

  (** Create a configuration from a sup-configuration and a subconfiguration by
      filtering the unused arguments in the sup-configuration. *)
  val to_conf : sup:conf -> t -> conf

  (** `drop_arg x` creates the list of all the sub-configurations than can be obtained
        by dropping an argument for one of the unknowns. *)
  val drop_arg : t -> ((int * int) * t) list

  (** `add_arg x` creates the list of all the sub-configurations than can be obtained
        by adding an argument for one of the unknowns. This requires having acess to the
        sup-configuration. *)
  val add_arg : sup:t -> t -> ((int * int) * t) list
end

module SubconfEdge : sig
  type t = Subconf.t * Subconf.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

val of_varset : Variables.VarSet.t -> 'a list Term.VarMap.t
val check_pmrs : Lang.PMRS.t -> bool

val base_type_args
  :  Env.env
  -> PMRS.t
  -> Variables.VarSet.t
  -> (term, Term.Terms.comparator_witness) Base.Set.t

(** Return the maximal configuration of a PMRS: the configuration where every unknown
    uses as many arguments as possible. *)
val max_configuration : Env.env -> PMRS.t -> conf

(** Returns the number of subconfigurations of a given configuration. *)
val subconf_count : conf -> int

(** Return the current configuration of a PMRS. *)
val configuration_of : PMRS.t -> conf

(** `same_conf p1 p2` is `true` iff `p1` and `p2` are in the same configuration:
    they have the same set of unknowns and the unknowns have the same set of arguments. *)
val same_conf : PMRS.t -> PMRS.t -> bool

(** `apply_configuration ~ctx conf p` applies configuration `conf` to `p` by
    ensuring the unknowns in `p` have the correct type.
    Returns a pair of a *copy* of `ctx` and the new PMRS. The copy of the
    environment is such that the unknown have the expected type, and all other
    types and variables names are unchanged.
*)
val apply_configuration : ctx:Env.env -> conf -> PMRS.t -> PMRS.t * Env.env

(** Count the number of recursive calls in a configuration.  *)
val num_rec_calls : ctx:Env.env -> conf -> int

val get_rstar : Env.env -> Psi.t -> int -> TermSet.t * TermSet.t
