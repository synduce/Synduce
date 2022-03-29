open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils

(**
  AState: This module contains state variables for the synthesis algorithms, as well as type
   definitions and printing functions for displaying solutions.
 *)

module PsiDef = struct
  (**
  PsiDef.t is the definition of the problem: the goal is to synthesize the unknowns in target such
  that target = orig ∘ repr
*)
  type t =
    { id : int (** An identifier for the problem. *)
    ; target : PMRS.t (**
  The target recursion skeleton in the problem.
  *)
    ; reference : PMRS.t (**
   The reference function in the problem.
  *)
    ; repr : PMRS.t (**
    The representation function in the problem.
  *)
    ; tinv : PMRS.t option
          (**
  The requires predicate of the target recursion skeleton.
  *)
    ; repr_is_identity : bool (**
  A boolean that is true iff psi_repr is identity.
  *)
    ; lifting : RType.t list
          (** The current lifting: the output type of psi_target
  should be !_alpha extended with the tuple of types psi_lifting. *)
    }

  let logics (p : t) = [ p.repr.plogic; p.reference.plogic; p.target.plogic ]
  let compare (p1 : t) (p2 : t) = compare p1.id p2.id
  let equal (p1 : t) (p2 : t) = compare p1 p2 = 0
  let hash (p : t) = p.id

  (* State variables for the algorithm. *)
  let psi_id : int ref = ref 0

  let new_psi_id () : int =
    let id = !psi_id in
    Int.incr psi_id;
    id
  ;;
end

(* ============================================================================================= *)
(*       Types for intermediate representations of solutions, lemmas, counterexamples, etc.      *)
(* ============================================================================================= *)

(** Representation of a solution, i.e. a function (as a PMRS) together with an association list
from unknown name to its implementation.
*)
type soln =
  { soln_rec_scheme : PMRS.t (** The recursion scheme this object is a solution of. *)
  ; soln_implems : (symbol * variable list * term) list
        (** The association list containing the implementation of each unknown. Each element of the
      list is a triple of unknown name, list of arguments, and body of the unknown implementation.
  *)
  }

(**
  Represents an equational constraint.
*)
type equation =
  { eterm : term (** The term from which the equation originates. *)
  ; eprecond : term option (** An optional precondition to the equation. *)
  ; esplitter : term option
  ; elhs : term (** The left-hand side of the equation, containing no unknowns. *)
  ; erhs : term (** The right-hand side of the equation, possibly with unknowns. *)
  ; eelim : (term * term) list (** The substitution used to eliminate recursion. *)
  }

type spurious_cause =
  | ViolatesTargetRequires
  | NotInReferenceImage
      (** A counterexample can be spurious either because it wiolates the target's requires or
  because some values assigned to recursion elimination variables are not in the reference
  function's image.
*)

type ctex_stat =
  | Valid
  | Spurious of spurious_cause list
  | Unknown
      (**
  A counterexample is either valid, or spurious with some reason, or unknown.
*)

(** A counterexample related to an equation and some info on the validity of the counterexample.
*)
type ctex =
  { ctex_eqn : equation (** The equation the counterexample relates to. *)
  ; ctex_vars : VarSet.t (** The variables in the model.*)
  ; ctex_model : term VarMap.t
        (** The model of the counterexample, mapping variables to terms. The terms should be
        constants. *)
  ; ctex_stat : ctex_stat (** The spuriousness status of the counterexample. *)
  }

(** A counterexample to realizability is a pair of models: a pair of maps from variable ids to terms. *)
type unrealizability_ctex =
  { i : int
  ; j : int
  ; ci : ctex
  ; cj : ctex
  }

type equation_system = equation list

(** Contains the last equation system used to solved the problem when a solution is found. *)
let solved_eqn_system : equation_system option ref = ref None

type term_state_detail =
  { term : term
  ; splitter : term option
  ; lemmas : term list
  ; lemma : variable
  ; lemma_candidate : term option
  ; negative_ctexs : ctex list
  ; positive_ctexs : ctex list
  ; recurs_elim : (term * term) list
  ; scalar_vars : variable list
  ; current_preconds : term option
  }

type term_state =
  (term * term option, term_state_detail, KeyedTerms.comparator_witness) Map.t
(*
  A type for lemmas.
  For now, it only contains a map from terms to terms.
  It is used during the generation of constraints from a set of terms.
  If a term t used to generate a constraint appears in the lemma map, then the lemmas lem_map(t)
  are added to the precondition of the constraint, and every subterm of the form
  (f(r(x)) or (target(x)) is replaced by a scalar (matching the substitution happending
  during the constraint generation).
 *)

(**
  The type to describe liftings.
  tmap is a map from terms to the expression of the lifting.
*)
type lifting = { tmap : ((int * term) * term) list }

(** The state of the main refinement loop. Currently, it is entirely determined by the sets T and U,
  accompanied with a set of lemmas that can be used during the generation of constraints.
 *)
type refinement_loop_state =
  { t_set : TermSet.t
  ; u_set : TermSet.t
  ; term_state : term_state
  ; lifting : lifting
  ; assumptions : equation list
  }

(** The algorithm may answere with realizable, in which case it provides a solution, unrealizable,
in which case it provides a list of counterexamples, or failure.
*)
type 'a segis_response =
  | Realizable of soln
  | Unrealizable of unrealizability_ctex list
  | Failed of 'a

(* ============================================================================================= *)
(*          Pretty printing functions.                                                           *)
(* ============================================================================================= *)

let pp_equation ~(ctx : Context.t) (f : Formatter.t) (eqn : equation) =
  match eqn.eprecond with
  | Some inv ->
    Fmt.(
      pf
        f
        "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a@;@[%a@;%a@;%a@]@]@]"
        (styled `Italic (pp_term ctx))
        eqn.eterm
        (pp_subs ctx)
        eqn.eelim
        (pp_term ctx)
        inv
        (styled (`Fg `Red) string)
        "=>"
        (pp_term ctx)
        eqn.elhs
        (styled (`Fg `Red) string)
        "="
        (pp_term ctx)
        eqn.erhs)
  | None ->
    Fmt.(
      pf
        f
        "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a %a@]@]"
        (styled `Italic (pp_term ctx))
        eqn.eterm
        (pp_subs ctx)
        eqn.eelim
        (pp_term ctx)
        eqn.elhs
        (styled (`Fg `Red) string)
        "="
        (pp_term ctx)
        eqn.erhs)
;;

let pp_ctex ~(ctx : Context.t) (f : Formatter.t) (ctex : ctex) : unit =
  let pp_model frmt model =
    (* Print as comma-separated list of variable -> term *)
    Fmt.(list ~sep:comma (pair ~sep:Utils.rightarrow (Variable.pp ctx) (pp_term ctx)))
      frmt
      (Map.to_alist model)
  in
  Fmt.pf
    f
    "@[M = [%a]@]@;@[for %a@]@;@[with elim. %a@]"
    pp_model
    ctex.ctex_model
    (pp_term ctx)
    ctex.ctex_eqn.eterm
    (pp_subs ctx)
    ctex.ctex_eqn.eelim
;;

let pp_implems
    ~(ctx : Context.t)
    (frmt : Formatter.t)
    (implems : (symbol * variable list * term) list)
  =
  let pp_single_or_tup frmt l =
    match l with
    | [] -> ()
    | [ (_, v) ] -> (pp_term ctx) frmt v
    | _ :: _ ->
      Fmt.(
        pf
          frmt
          "(%a)"
          (list ~sep:(fun _f () -> pf _f ", ") (pp_term ctx))
          (List.map ~f:second l))
  in
  let pp_implem frmt (s, args, t) =
    let f v =
      match Variable.vtype_or_new ctx v with
      | TTup tl ->
        let f i typ =
          let v' = Variable.mk ctx ~t:(Some typ) (Alpha.fresh ctx.names ~s:v.vname) in
          mk_sel ctx (mk_var ctx v) i, mk_var ctx v'
        in
        let subs1 = List.mapi ~f tl in
        let _, vars = List.unzip subs1 in
        (mk_var ctx v, mk_tup ctx vars) :: subs1, subs1
      | _ -> [], [ mk_var ctx v, mk_var ctx v ]
    in
    let subs, args = List.unzip (List.map ~f args) in
    let t' = substitution (List.concat subs) t in
    Fmt.(
      pf
        frmt
        "@[<hov 2>%a %a @[%a@] %a@;%a@]"
        (styled (`Fg `Red) string)
        "let"
        (styled (`Fg `Cyan) string)
        s
        (list ~sep:sp pp_single_or_tup)
        args
        (styled (`Fg `Red) string)
        "="
        (pp_term ctx)
        t')
  in
  Fmt.((list ~sep:(fun _f () -> pf _f "@.@.") pp_implem) frmt implems)
;;

let pp_soln
    ?(use_ocaml_syntax = false)
    ~(ctx : Context.t)
    (frmt : Formatter.t)
    (solution : soln)
  =
  Fmt.(
    pf
      frmt
      "@.%a@.@.@[%a@]@."
      (pp_implems ~ctx)
      solution.soln_implems
      (if use_ocaml_syntax
      then PMRS.pp_ocaml ~ctx ~short:false
      else PMRS.pp ~ctx ~short:false)
      solution.soln_rec_scheme)
;;
