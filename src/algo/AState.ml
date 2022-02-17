open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils
open Domainslib

(**
  AState: This module contains state variables for the synthesis algorithms, as well as type
   definitions and printing functions for displaying solutions.
 *)

module Context = struct
  exception Escape

  type t =
    { mutable c_alive : bool
    ; mutable c_pid : int
    ; mutable c_subctx : t array
    ; c_chan : int Chan.t
    }

  let mk () =
    { c_alive = true; c_pid = -1; c_subctx = [||]; c_chan = Chan.make_bounded 10 }
  ;;

  let send_termination (ctx : t) = Chan.send ctx.c_chan (-1)

  let subkill (ctx : t) : unit =
    Array.iter ~f:(fun subctx -> Unix.kill subctx.c_pid Caml.Sys.sigkill) ctx.c_subctx;
    if ctx.c_pid > 0 then Unix.kill ctx.c_pid Caml.Sys.sigkill
  ;;

  let check (ctx : t) =
    match Chan.recv_poll ctx.c_chan with
    | Some i ->
      if i < 0
      then (
        subkill ctx;
        raise Escape)
      else ()
    | None -> ()
  ;;
end

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

(** The type τ in the paper, input type of the reference function.  *)
let _tau = ref RType.TInt

(** The type θ in the paper, input type of the target recursion skeleton.  *)
let _theta = ref RType.TInt

(** The type D in the paper: the output type of the reference and the target recursion skeleton. The
    first element is the pure type output type of the functions, and the second element is an
    optional term that represents the additional predicate on the output of the reference function.
    The term is assumed to define a function (fun (free variables of term) -> term).
*)
let _alpha : RType.t ref = ref RType.TInt

(** Not useful for now. *)
let _span = ref 1

let refinement_steps = ref 0
let secondary_refinement_steps = ref 0

let reinit () =
  _span := 1;
  refinement_steps := 0;
  _alpha := RType.TInt;
  _tau := RType.TInt;
  _theta := RType.TInt
;;

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

let pp_equation (f : Formatter.t) (eqn : equation) =
  match eqn.eprecond with
  | Some inv ->
    Fmt.(
      pf
        f
        "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a@;@[%a@;%a@;%a@]@]@]"
        (styled `Italic pp_term)
        eqn.eterm
        pp_subs
        eqn.eelim
        pp_term
        inv
        (styled (`Fg `Red) string)
        "=>"
        pp_term
        eqn.elhs
        (styled (`Fg `Red) string)
        "="
        pp_term
        eqn.erhs)
  | None ->
    Fmt.(
      pf
        f
        "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a %a@]@]"
        (styled `Italic pp_term)
        eqn.eterm
        pp_subs
        eqn.eelim
        pp_term
        eqn.elhs
        (styled (`Fg `Red) string)
        "="
        pp_term
        eqn.erhs)
;;

let pp_ctex (f : Formatter.t) (ctex : ctex) : unit =
  let pp_model frmt model =
    (* Print as comma-separated list of variable -> term *)
    Fmt.(list ~sep:comma (pair ~sep:Utils.rightarrow Variable.pp pp_term))
      frmt
      (Map.to_alist model)
  in
  Fmt.pf
    f
    "@[M = [%a]@]@;@[for %a@]@;@[with elim. %a@]"
    pp_model
    ctex.ctex_model
    pp_term
    ctex.ctex_eqn.eterm
    pp_subs
    ctex.ctex_eqn.eelim
;;

let pp_implems (frmt : Formatter.t) (implems : (symbol * variable list * term) list) =
  let pp_single_or_tup frmt l =
    match l with
    | [] -> ()
    | [ (_, v) ] -> pp_term frmt v
    | _ :: _ ->
      Fmt.(
        pf
          frmt
          "(%a)"
          (list ~sep:(fun _f () -> pf _f ", ") pp_term)
          (List.map ~f:second l))
  in
  let pp_implem frmt (s, args, t) =
    let f v =
      match Variable.vtype_or_new v with
      | TTup tl ->
        let f i typ =
          let v' = Variable.mk ~t:(Some typ) (Alpha.fresh ~s:v.vname ()) in
          mk_sel (mk_var v) i, mk_var v'
        in
        let subs1 = List.mapi ~f tl in
        let _, vars = List.unzip subs1 in
        (mk_var v, mk_tup vars) :: subs1, subs1
      | _ -> [], [ mk_var v, mk_var v ]
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
        pp_term
        t')
  in
  Fmt.((list ~sep:(fun _f () -> pf _f "@.@.") pp_implem) frmt implems)
;;

let pp_soln ?(use_ocaml_syntax = false) (frmt : Formatter.t) (solution : soln) =
  Fmt.(
    pf
      frmt
      "@.%a@.@.@[%a@]@."
      pp_implems
      solution.soln_implems
      (if use_ocaml_syntax then PMRS.pp_ocaml ~short:false else PMRS.pp ~short:false)
      solution.soln_rec_scheme)
;;
