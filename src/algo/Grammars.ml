open Base
open Lang
open Lang.Term
open Lang.Rewriter
open Syguslib.Sygus
open Lang.SygusInterface
open Utils

type grammar_parameters =
  { g_opset : OpSet.t
  ; g_fixed_constants : bool
  ; g_mul_constant : bool
  ; g_nonlinear : bool
  ; g_locals : (sygus_term * sygus_sort) list
  ; g_bools : bool
  }

let preamble
    (grammar_params : grammar_parameters)
    (ret_sort : sygus_sort)
    (gguess : Skeleton.t)
    ~(ints : sygus_term)
    ~(bools : sygus_term)
  =
  match grammar_production_of_skeleton ~ints ~bools gguess grammar_params.g_locals with
  | [] -> []
  | guesses -> [ ("IStart", ret_sort), List.map ~f:(fun x -> GTerm x) guesses ]
;;

let int_sort = SId (IdSimple "Int")
let bool_sort = SId (IdSimple "Bool")
let int_nonterm = SyId (IdSimple "Ix")
let int_const_nonterm = SyId (IdSimple "Ic")
let bool_nonterm = SyId (IdSimple "Ipred")

let int_base : grammar_def =
  [ ( ("Ix", int_sort)
    , [ GTerm int_const_nonterm
      ; GVar int_sort
      ; GTerm (SyApp (IdSimple "-", [ int_nonterm ]))
      ; GTerm (SyApp (IdSimple "+", [ int_nonterm ]))
      ; GTerm (SyApp (IdSimple "min", [ int_nonterm; int_nonterm ]))
      ; GTerm (SyApp (IdSimple "max", [ int_nonterm; int_nonterm ]))
      ; GTerm (SyApp (IdSimple "*", [ int_const_nonterm; int_nonterm ]))
      ; GTerm (SyApp (IdSimple "div", [ int_nonterm; int_const_nonterm ]))
      ; GTerm (SyApp (IdSimple "abs", [ int_nonterm ]))
      ; GTerm (SyApp (IdSimple "div", [ int_nonterm; int_const_nonterm ]))
      ; GTerm (SyApp (IdSimple "ite", [ bool_nonterm; int_nonterm; int_nonterm ]))
      ] )
  ; ("Ic", int_sort), [ GConstant int_sort ]
  ; ( ("Ipred", bool_sort)
    , [ GTerm (SyApp (IdSimple "=", [ int_nonterm; int_nonterm ]))
      ; GTerm (SyApp (IdSimple ">", [ int_nonterm; int_nonterm ]))
      ; GTerm (SyApp (IdSimple "not", [ bool_nonterm ]))
      ; GTerm (SyApp (IdSimple "and", [ bool_nonterm; bool_nonterm ]))
      ; GTerm (SyApp (IdSimple "or", [ bool_nonterm; bool_nonterm ]))
      ] )
  ]
;;

let int_parametric ?(guess = None) (params : grammar_parameters) =
  let main_grammar =
    [ ( ("Ix", int_sort)
      , [ GTerm int_const_nonterm ]
        @ List.filter_map params.g_locals ~f:(fun (t, s) ->
              match s with
              | SId (IdSimple "Int") -> Some (GTerm t)
              | _ -> None)
        @ [ GTerm (SyApp (IdSimple "-", [ int_nonterm ])) ]
        @ [ GTerm (SyApp (IdSimple "+", [ int_nonterm; int_nonterm ])) ]
        @ (if (not params.g_nonlinear) && params.g_mul_constant
          then
            [ GTerm (SyApp (IdSimple "*", [ int_const_nonterm; int_nonterm ]))
            ; GTerm (SyApp (IdSimple "div", [ int_nonterm; int_const_nonterm ]))
            ]
          else [])
        @ (if Set.mem params.g_opset (Binary Min)
          then [ GTerm (SyApp (IdSimple "min", [ int_nonterm; int_nonterm ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Max)
          then [ GTerm (SyApp (IdSimple "max", [ int_nonterm; int_nonterm ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Times)
              || Set.mem params.g_opset (Binary Div)
              || params.g_nonlinear
          then
            [ GTerm (SyApp (IdSimple "*", [ int_nonterm; int_nonterm ]))
            ; GTerm (SyApp (IdSimple "div", [ int_nonterm; int_nonterm ]))
            ]
          else [])
        @ (if Set.mem params.g_opset (Unary Abs)
          then [ GTerm (SyApp (IdSimple "abs", [ int_nonterm ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Div)
          then [ GTerm (SyApp (IdSimple "div", [ int_nonterm; int_const_nonterm ])) ]
          else [])
        @
        if params.g_bools
        then
          [ GTerm (SyApp (IdSimple "ite", [ bool_nonterm; int_nonterm; int_nonterm ])) ]
        else [] )
    ]
    @ [ ( ("Ic", int_sort)
        , if params.g_fixed_constants
          then [ GTerm (SyLit (LitNum 0)); GTerm (SyLit (LitNum 1)) ]
          else [ GConstant int_sort ] )
      ]
    @
    if params.g_bools
    then
      [ ( ("Ipred", bool_sort)
        , List.filter_map params.g_locals ~f:(fun (t, s) ->
              match s with
              | SId (IdSimple "Bool") -> Some (GTerm t)
              | _ -> None)
          @ [ GTerm (SyApp (IdSimple "=", [ int_nonterm; int_nonterm ]))
            ; GTerm (SyApp (IdSimple ">", [ int_nonterm; int_nonterm ]))
            ; GTerm (SyApp (IdSimple "not", [ bool_nonterm ]))
            ; GTerm (SyApp (IdSimple "and", [ bool_nonterm; bool_nonterm ]))
            ; GTerm (SyApp (IdSimple "or", [ bool_nonterm; bool_nonterm ]))
            ] )
      ]
    else []
  in
  match guess with
  | None -> main_grammar
  | Some gguess ->
    preamble params int_sort ~ints:int_nonterm ~bools:bool_nonterm gguess @ main_grammar
;;

let bool_parametric
    ?(guess = None)
    ?(special_const_prod = true)
    ?(short_predicate = false)
    (params : grammar_parameters)
  =
  let has_ints =
    List.exists params.g_locals ~f:(fun (_, s) ->
        match s with
        | SId (IdSimple "Int") -> true
        | _ -> false)
  in
  let bool_section =
    [ ( ("Ipred", bool_sort)
      , List.filter_map params.g_locals ~f:(fun (t, s) ->
            match s with
            | SId (IdSimple "Bool") -> Some (GTerm t)
            | _ -> None)
        @ [ GTerm (SyApp (IdSimple "not", [ bool_nonterm ]))
          ; GTerm (SyApp (IdSimple "and", [ bool_nonterm; bool_nonterm ]))
          ; GTerm (SyApp (IdSimple "or", [ bool_nonterm; bool_nonterm ]))
          ]
        @ (if List.length params.g_locals <= 0
          then [ GTerm (SyId (IdSimple "true")); GTerm (SyId (IdSimple "false")) ]
          else [])
        @
        if has_ints
        then
          (if special_const_prod
          then [ GTerm (SyApp (IdSimple "=", [ int_nonterm; int_const_nonterm ])) ]
          else [])
          @ [ GTerm (SyApp (IdSimple "=", [ int_nonterm; int_nonterm ]))
            ; GTerm (SyApp (IdSimple ">", [ int_nonterm; int_nonterm ]))
            ; GTerm (SyApp (IdSimple ">=", [ int_nonterm; int_nonterm ]))
            ]
        else [] )
    ]
  in
  let int_section =
    [ ( ("Ix", int_sort)
      , [ GTerm int_const_nonterm ]
        @ List.filter_map params.g_locals ~f:(fun (t, s) ->
              match s with
              | SId (IdSimple "Int") -> Some (GTerm t)
              | _ -> None)
        @ (if special_const_prod
          then [ GTerm (SyApp (IdSimple "+", [ int_nonterm; int_const_nonterm ])) ]
          else [])
        @ (if not short_predicate
          then
            [ GTerm (SyApp (IdSimple "-", [ int_nonterm ])) ]
            @ [ GTerm (SyApp (IdSimple "+", [ int_nonterm; int_nonterm ])) ]
            @ (if Set.mem params.g_opset (Binary Min)
              then [ GTerm (SyApp (IdSimple "min", [ int_nonterm; int_nonterm ])) ]
              else [])
            @
            if Set.mem params.g_opset (Binary Max)
            then [ GTerm (SyApp (IdSimple "max", [ int_nonterm; int_nonterm ])) ]
            else []
          else [])
        @ (if Set.mem params.g_opset (Binary Times)
          then [ GTerm (SyApp (IdSimple "*", [ int_const_nonterm; int_nonterm ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Div)
          then [ GTerm (SyApp (IdSimple "div", [ int_nonterm; int_const_nonterm ])) ]
          else [])
        @ (if Set.mem params.g_opset (Unary Abs)
          then [ GTerm (SyApp (IdSimple "abs", [ int_nonterm ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Div)
          then [ GTerm (SyApp (IdSimple "div", [ int_nonterm; int_const_nonterm ])) ]
          else [])
        @ [ GTerm (SyApp (IdSimple "ite", [ bool_nonterm; int_nonterm; int_nonterm ])) ]
        @ (if Set.mem params.g_opset (Binary Mod)
          then [ GTerm (SyApp (IdSimple "mod", [ int_nonterm; int_nonterm ])) ]
          else [])
        @
        if Set.mem params.g_opset (Binary Times)
           || Set.mem params.g_opset (Binary Div)
           || params.g_nonlinear
        then
          [ GTerm (SyApp (IdSimple "*", [ int_nonterm; int_nonterm ]))
          ; GTerm (SyApp (IdSimple "div", [ int_nonterm; int_nonterm ]))
          ]
        else [] )
    ]
    @ [ ( ("Ic", int_sort)
        , if params.g_fixed_constants
          then [ GTerm (SyLit (LitNum 0)); GTerm (SyLit (LitNum 1)) ]
          else [ GConstant int_sort ] )
      ]
  in
  let main_grammar = if has_ints then bool_section @ int_section else bool_section in
  match guess with
  | None -> main_grammar
  | Some gguess ->
    preamble params bool_sort ~ints:int_nonterm ~bools:bool_nonterm gguess @ main_grammar
;;

let tuple_grammar_constr (params : grammar_parameters) (types : RType.t list) =
  let use_bool = ref params.g_bools in
  let tuple_args =
    List.map types ~f:(function
        | TInt -> int_nonterm
        | TBool ->
          use_bool := true;
          bool_nonterm
        | _ -> failwith "TODO: complex tuples not supported.")
  in
  let head_rule =
    if !Config.using_cvc4_tuples
    then
      ( ("Tr", SApp (IdSimple "Tuple", List.map ~f:SygusInterface.sort_of_rtype types))
      , [ GTerm (SyApp (IdSimple "mkTuple", tuple_args)) ] )
    else (
      let tuple_type_name = Tuples.type_name_of_types types in
      let constr_name = Tuples.constr_name_of_types types in
      ( ("Tr", SId (IdSimple tuple_type_name))
      , [ GTerm (SyApp (IdSimple constr_name, tuple_args)) ] ))
  in
  head_rule :: int_parametric { params with g_bools = !use_bool }
;;

let grammar_sort_decomp (sort : sygus_sort) =
  match sort with
  | SId (IdSimple "Int") -> Some int_base
  | SId (IdSimple "Bool") -> None
  | SApp (IdSimple "Tuple", _) -> None
  | _ -> None
;;

let rec project (v : variable) =
  let rec aux =
    RType.(
      function
      | (TInt | TBool) as t -> [ SyId (IdSimple v.vname), sort_of_rtype t ]
      | TParam _ -> [ SyId (IdSimple v.vname), int_sort ]
      | TTup types ->
        let l = List.map ~f:aux types in
        List.concat (List.mapi l ~f:(tuple_sel types))
      | _ as t -> [ SyId (IdSimple v.vname), sort_of_rtype t ])
  in
  aux (Variable.vtype_or_new v)

and tuple_sel types i projs =
  if !Config.using_cvc4_tuples
  then
    List.map projs ~f:(fun (t, s) -> SyApp (IdIndexed ("tupSel", [ INum i ]), [ t ]), s)
  else (
    let tuple_selector = Tuples.proj_name_of_types types i in
    List.map projs ~f:(fun (t, s) -> SyApp (IdSimple tuple_selector, [ t ]), s))
;;

let generate_grammar
    ?(nonlinear = false)
    ?(guess = None)
    ?(bools = false)
    ?(special_const_prod = true)
    ?(short_predicate = false)
    (opset : OpSet.t)
    (args : variable list)
    (ret_sort : RType.t)
  =
  let locals_of_scalar_type = List.concat (List.map ~f:project args) in
  let params =
    { g_opset = opset
    ; g_fixed_constants = false
    ; g_mul_constant = false
    ; g_nonlinear = nonlinear
    ; g_locals = locals_of_scalar_type
    ; g_bools = bools || not (Set.are_disjoint opset OpSet.comparison_operators)
    }
  in
  if !Config.no_grammar_for_constants && List.length args = 0
  then (* In some cases the default grammar turns out to be faster. *)
    None
  else (
    match ret_sort with
    | TInt -> Some (int_parametric ~guess params)
    | TBool -> Some (bool_parametric ~short_predicate ~guess ~special_const_prod params)
    (* Ignore guess if it's a tuple that needs to be generated. *)
    | TTup types -> Some (tuple_grammar_constr params types)
    | _ -> None)
;;

(* ============================================================================================= *)
(*                          GRAMMAR OPTIMIZATION                                                 *)
(* ============================================================================================= *)

let make_basic_guess (eqns : (term * term option * term * term) list) (xi : variable)
    : [> `First of symbol * variable list * term | `Second of Skeleton.t | `Third ]
  =
  let lhs_of_xi =
    let f (_, _, lhs, rhs) =
      match rhs.tkind with
      | TApp ({ tkind = TVar x; _ }, _) -> if Variable.(xi = x) then Some lhs else None
      | _ -> None
    in
    List.filter_map ~f eqns
  in
  let guesses =
    let f lhs =
      let t = lhs.ttyp in
      Skeleton.(
        match lhs.tkind with
        | TBin (op, _, _) ->
          Some
            (match
               List.map
                 ~f:(fun (ta, tb) -> SBin (op, SType ta, SType tb))
                 (Binop.operand_types op)
             with
            | [ a ] -> a
            | _ as l -> SChoice l)
        | TUn (op, _) -> Some (SUn (op, SType (Unop.operand_type op)))
        | TIte (_, _, _) -> Some (SIte (SType Lang.RType.TBool, SType t, SType t))
        | TConst _ -> None
        | TVar _ -> None
        | _ -> Some SNonGuessable)
    in
    List.filter_map ~f lhs_of_xi
  in
  match guesses with
  | [] -> `Third
  | hd :: tl ->
    if List.for_all tl ~f:(fun x -> Poly.equal x hd) then `Second hd else `Third
;;

let make_unification_guess
    (eqns : (term * term option * term * term) list)
    (xi : variable)
    : [> `First of string * variable list * term | `Second of Skeleton.t | `Third ]
  =
  match Deduction.Solver.presolve_equations ~xi eqns with
  | `Third -> make_basic_guess eqns xi
  | _ as l -> l
;;

let make_guess
    ?(level = 1)
    (xi : variable)
    (eqns : (term * term option * term * term) list)
    : [> `First of symbol * variable list * term | `Second of Skeleton.t | `Third ]
  =
  match level with
  | 0 -> `Third
  | 1 -> make_basic_guess eqns xi
  | 2 -> make_unification_guess eqns xi
  | _ -> `Third
;;
