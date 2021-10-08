val get_id_const : Term.Binop.t -> Term.term option
val is_id_int_const : Term.Binop.t -> int -> bool
val is_and : Term.Binop.t -> bool
val is_or : Term.Binop.t -> bool
val get_ty_const : RType.t -> Term.term
val is_left_distrib : Term.Operator.t -> Term.Operator.t -> bool
val get_distributing : Term.Operator.t -> Term.Operator.t
val is_right_distrib : Term.Binop.t -> Term.Binop.t -> bool
val is_assoc : Term.Binop.t -> bool
val mk_assoc_with_id : Term.Binop.t -> Term.term list -> Term.term option
val is_commutative : Term.Operator.t -> bool
val concrete_int_op : Term.Operator.t -> (int -> int -> int) option

module IS : sig
  type t = Base.Set.M(Base.Int).t
  type elt = int

  val empty : (elt, Base.Int.comparator_witness) Base.Set.t
  val singleton : elt -> (elt, Base.Int.comparator_witness) Base.Set.t

  (** Make a set of integers from a list of integers. *)
  val of_list : elt list -> (elt, Base.Int.comparator_witness) Base.Set.t

  (** Set union. *)
  val ( + ) : ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t

  (** Set difference. *)
  val ( - ) : ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t

  (**  Set intersection. *)
  val ( ^ ) : ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t

  (** Set emptiness. *)
  val ( ?. ) : ('a, 'b) Base.Set.t -> bool

  val ( ~$ ) : elt -> (elt, Base.Int.comparator_witness) Base.Set.t
  val pp : Format.formatter -> t -> unit
end

module Expression : sig
  val box_id : int ref
  val new_box_id : unit -> int

  type t =
    | ETrue
    | EFalse
    | EInt of int
    | EVar of int
    | EBox of int
    | ETup of t list
    | EIte of t * t * t
    | EData of string * t list
    | EOp of Term.Operator.t * t list

  val register_var : Term.variable -> unit
  val get_var : int -> Term.variable option
  val pp_ivar : Format.formatter -> int -> unit
  val pp_ivarset : Format.formatter -> IS.t -> unit
  val pp : t Fmt.t
  val mk_e_true : t
  val mk_e_false : t
  val mk_e_int : int -> t
  val mk_e_bool : bool -> t
  val mk_e_var : int -> t
  val mk_e_tup : t list -> t
  val mk_e_assoc : Term.Operator.t -> t list -> t
  val mk_e_ite : t -> t -> t -> t
  val mk_e_data : string -> t list -> t
  val mk_e_bin : Term.Binop.t -> t -> t -> t
  val mk_e_un : Term.Unop.t -> t -> t

  module Op : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( && ) : t -> t -> t
    val ( || ) : t -> t -> t
    val max : t -> t -> t
    val min : t -> t -> t
    val not : t -> t
    val int : int -> t
    val var : int -> t
    val ( ~? ) : t -> t -> t -> t
  end

  val reduce
    :  case:((t -> 'a) -> t -> 'a option)
    -> join:('a -> 'a -> 'a)
    -> init:'a
    -> t
    -> 'a

  val transform : ((t -> t) -> t -> t option) -> t -> t
  val rewrite_until_stable : (t -> t) -> t -> t
  val expr_size : t -> int
  val expr_size_compare : t -> t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val free_variables : t -> (int, Base.Int.comparator_witness) Base.Set.t
  val alpha_equal : t -> t -> bool
  val of_term : Term.term -> t option
  val to_term : t -> Term.term option
  val simplify : t -> t
  val normalize : t -> t
  val get_id_const : Term.Operator.t -> t option
  val get_ty_const : RType.t -> t
end

val factorize : Expression.t -> Expression.t
val distrib : Term.Operator.t -> Expression.t list -> Expression.t
val expand : Expression.t -> Expression.t
val rewrite_with_lemma : Expression.t -> Expression.t -> Expression.t list

val match_as_subexpr
  :  ?lemma:Expression.t option
  -> Expression.t
  -> of_:Expression.t
  -> (int * Expression.t) option
