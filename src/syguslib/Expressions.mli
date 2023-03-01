(** Convenience module for writing expressions within predefined theories.  *)

open Sygus

(* Terminals *)

(** Create a term containing a variable given a variable name. *)
val var : symbol -> sygus_term

(** Create a sort given a sort name. *)
val sort : symbol -> sygus_sort

(** Create an integer constant term given an int. *)
val int : int -> sygus_term

(** Create a decimal number term given a float. *)
val real : float -> sygus_term

(** Create a hex constant given its string representation. *)
val hex : string -> sygus_term

(* If-then-else *)

(** Create a conditional given the condition, then-branch and else-branch expressions.  *)
val ite : sygus_term -> sygus_term -> sygus_term -> sygus_term

(* Arithmetic *)

(** Numeral division.  *)
val ( / ) : sygus_term -> sygus_term -> sygus_term

(** Numeral addition. *)
val ( + ) : sygus_term -> sygus_term -> sygus_term

(** Numeral Substraction.  *)
val ( - ) : sygus_term -> sygus_term -> sygus_term

(** Numeral multiplication. *)
val ( * ) : sygus_term -> sygus_term -> sygus_term

(** Max function (non-standard, needs to be declared first in a SyGuS file.)  *)
val max : sygus_term -> sygus_term -> sygus_term

(** Min function (non-standard, needs to be declared first in a SyGuS file.)  *)
val min : sygus_term -> sygus_term -> sygus_term

(** Modulo operation.  *)
val modulo : sygus_term -> sygus_term -> sygus_term

(** Abs operations.  *)
val abs : sygus_term -> sygus_term

(** Unary minus.  *)
val neg : sygus_term -> sygus_term

(* Bool *)

(** The false value.  *)
val mk_false : sygus_term

(** The true value in Sygus.  *)
val mk_true : sygus_term

(** Conjunction.  *)
val ( && ) : sygus_term -> sygus_term -> sygus_term

(** Disjunction. *)
val ( || ) : sygus_term -> sygus_term -> sygus_term

(** Negation.  *)
val not : sygus_term -> sygus_term

(* Comparisons *)
val ( > ) : sygus_term -> sygus_term -> sygus_term
val ( >= ) : sygus_term -> sygus_term -> sygus_term
val ( < ) : sygus_term -> sygus_term -> sygus_term
val ( <= ) : sygus_term -> sygus_term -> sygus_term
val ( = ) : sygus_term -> sygus_term -> sygus_term

(* Grammar *)

(** [gconst x] is the sygus grammar term [(Constant x)] *)
val gconst : sygus_sort -> sygus_gsterm

(** [gterm x] is the sygus grammar term [x] *)
val gterm : sygus_term -> sygus_gsterm

(** [gvar x] is the sygus grammar term [(Variable x)]*)
val gvar : sygus_sort -> sygus_gsterm

(** [glblock n s productions] defines the grammar productions with name [n]
    and sort [s] with the productions [productions].
  *)
val gblock : string -> sygus_sort -> sygus_gsterm list -> grammar_def

(* Predefined Sorts *)

(** The standard sort for integers.  *)
val int_sort : sygus_sort

(** The standard sort for booleans.  *)
val bool_sort : sygus_sort

(** The standard sort for reals. *)
val real_sort : sygus_sort
