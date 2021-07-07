open Base
open Term

val get_id_const : Binop.t -> term option
(** If the binary operator [op] has an identity element, then [get_id_const op] returns
  [Some term] where term is the identity of [op]
 *)

val get_ty_const : RType.t -> term

val is_left_distrib : Binop.t -> Binop.t -> bool
(** For example, [is_left_distrib Plus Max] is true. *)

val is_right_distrib : Binop.t -> Binop.t -> bool
(** For example, [is_right_distrib Plus Max] is true. *)

val is_assoc : Binop.t -> bool
(** For example, [is_assoc Plus] is true.  *)

val mk_assoc_with_id : Binop.t -> term list -> term option
(** [mk_assoc_with_id op args] Construct the term corresponding to the application of the associative
operator [op] to the arguments [args]. If [op] is not associative or has no identity element, then
  returns [None]. *)

val is_commutative : Binop.t -> bool
(** For example [is_commutative Times] returns true. *)

(** Set of ints = set of variables. Module is only meant to avoid (Set.... (module Int)) everywhere. *)
module IS : sig
  type t = Set.M(Int).t

  type elt = int

  val empty : t

  val singleton : int -> t

  val of_list : int list -> t

  val ( + ) : t -> t -> t
  (** Set union. *)

  val ( - ) : t -> t -> t
  (** Set difference. *)

  val ( ^ ) : t -> t -> t
  (** Set intersection. *)

  val ( ?. ) : t -> bool
  (** Is the set empty? *)

  val ( ~$ ) : int -> t
  (** Set singleton. *)

  val pp : Formatter.t -> t -> unit
end

(** The module for expressions: terms without let-bindings or functions. Useful rewriting and
  expression manipulation while ignoring "functional" features of the language.
 *)
module Expression : sig
  type t =
    | ETrue  (** The boolean constant true. *)
    | EFalse  (** The boolean constnat false. *)
    | EInt of int  (** The ineger constants. *)
    | EVar of int
        (** A variable, identified by its id. The id should be registered in _VARS through register_var. *)
    | EBox of int  (** A box with an id.*)
    | ETup of t list  (** A tuple of expressions. *)
    | EIte of t * t * t  (** A conditional expression (if .. then .. else) *)
    | EData of string * t list  (** A data constructor. *)
    | EAssoc of Binop.t * t list  (** An associative operator application. *)
    | EBin of Binop.t * t * t  (** A binary operator application. *)
    | EUn of Unop.t * t  (** A unary operator application. *)

  val pp : Formatter.t -> t -> unit

  val pp_ivar : Formatter.t -> int -> unit

  val equal : t -> t -> bool

  val mk_e_true : t
  (** The boolean constant true. *)

  val mk_e_false : t
  (** The boolean constant false. *)

  val mk_e_int : int -> t
  (** Constructs an integer constant. *)

  val mk_e_var : int -> t
  (** Construct a variable with an id. *)

  val mk_e_tup : t list -> t
  (** Construct a tuple of expressions. *)

  val mk_e_assoc : Binop.t -> t list -> t
  (** Construct an associative operator application. *)

  val mk_e_ite : t -> t -> t -> t
  (** Construct a conditional expression. *)

  val mk_e_data : string -> t list -> t
  (** Construct a datatype expression. *)

  val mk_e_bin : Term.Binop.t -> t -> t -> t
  (** Construct a binary operator application.  *)

  val mk_e_un : Term.Unop.t -> t -> t
  (** Construct a unary operator application. *)

  val register_var : Term.variable -> unit
  (** Register a variable by its id. *)

  val get_var : int -> variable option
  (** [get_var id] returns [Some variable] if the variable with id [id] has been registered,
        otherwise returns [None] *)

  val of_term : Term.term -> t option
  (** Construct an expression from a term. Returns [None] if the term is not an expression (e.g. contains
    lambda expressions, let-bindings, tuple projections)
  *)

  val to_term : t -> Term.term option
  (** Convert an expression to a term. Returns [None] if some variable id in the expression is not
  registered.
   *)

  val reduce : case:((t -> 'a) -> t -> 'a option) -> join:('a -> 'a -> 'a) -> init:'a -> t -> 'a
  (** Reduce an expression, using special case [case]. *)

  val transform : ((t -> t) -> t -> t option) -> t -> t
  (** Transform an expression.*)

  val free_variables : t -> IS.t

  val match_as_subexpr : t -> of_:t -> (int * t) option

  val get_id_const : Binop.t -> t option

  val get_ty_const : RType.t -> t
end
