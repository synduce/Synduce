open Base
open Term
open Smtlib

let rec theory_of (t : RType.t) =
  RType.(
    match t with
    | TInt -> Logics.Ints
    | TChar -> Logics.Ints
    | TString -> Logics.Strings
    | TBool -> Logics.Core
    | TNamed _s -> Logics.Core
    | TSet _ -> Logics.All
    | TTup tl ->
      List.fold ~f:(fun a b -> Logics.join_theories a (theory_of b)) ~init:Logics.Core tl
    | TVar _ -> Logics.Core
    | TFun (a, b) -> Logics.(join_theories (theory_of a) (theory_of b))
    | TParam (_, b) -> theory_of b)
;;

type logic_info =
  { theory : Logics.theory
  ; linearity : bool
  ; datatypes : bool
  }

let logic_info_join (linfo1 : logic_info) (linfo2 : logic_info) : logic_info =
  { theory = Logics.join_theories linfo1.theory linfo2.theory
  ; linearity = linfo1.linearity && linfo2.linearity
  ; datatypes = linfo1.datatypes || linfo2.datatypes
  }
;;

let base_logic_info = { theory = Logics.Core; linearity = true; datatypes = false }

let term_requires_datatype ~(ctx : Context.t) (t : term) =
  List.exists
    ~f:(fun x ->
      match Variable.vtype_or_new ctx x with
      | RType.TTup _ -> true
      | _ -> false)
    (Set.elements (Analysis.free_variables ~ctx ~include_functions:false t))
;;

(** Infer the logic to use in calls using terms.
    @param quantifier_free is true by default, set to false if quantifiers will be used
    in formulas.
    @param with_uninterpreted_functions is false by default, set to true if uninterpreted
    functions will be used.
    @param hint is an optional logic hint. The current implementation returns exaclty this
    hint but this will change.
    @param logic_infos a list of logic_infos, possibly from context program
*)
let infer_logic
    ?(quantifier_free = true)
    ?(with_uninterpreted_functions = false)
    ?(for_induction = false)
    ?(hint = None)
    ?(logic_infos = [])
    ~(ctx : Context.t)
    (terms : term list)
    : Logics.logic
  =
  let linfo = List.fold ~init:base_logic_info ~f:logic_info_join logic_infos in
  let linfo =
    let f linfo_accum t =
      let linearity = Set.for_all ~f:Operator.is_lia (Analysis.operators_of t) in
      let theory = theory_of (type_of t) in
      let datatypes = term_requires_datatype ~ctx t in
      logic_info_join linfo_accum { theory; linearity; datatypes }
    in
    List.fold ~f ~init:linfo terms
  in
  match hint with
  | Some s -> Logics.of_string s
  | None ->
    Logics.combine
      ~arith:true
      ~datatypes:(for_induction || linfo.datatypes)
      ~uf:with_uninterpreted_functions
      ~linear:linfo.linearity
      ~qf:quantifier_free
      [ linfo.theory ]
;;
