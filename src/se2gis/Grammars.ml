open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Lang.SygusInterface
open Utils
module E = Syguslib.Expressions

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
  | guesses -> E.gblock "IStart" ret_sort (List.map ~f:(fun x -> E.gterm x) guesses)
;;

let int_nonterm = E.var "Ix"
and int_const_nonterm = E.var "Ic"
and bool_nonterm = E.var "Ipred"

let int_base : grammar_def =
  E.(
    gblock
      "Ix"
      int_sort
      [ gterm int_const_nonterm
      ; gvar int_sort
      ; gterm (neg int_nonterm)
      ; gterm int_nonterm
      ; gterm (min int_nonterm int_nonterm)
      ; gterm (max int_nonterm int_nonterm)
      ; gterm (int_const_nonterm * int_nonterm)
      ; gterm (int_nonterm / int_const_nonterm)
      ; gterm (abs int_nonterm)
      ; gterm (ite bool_nonterm int_nonterm int_nonterm)
      ]
    @ gblock "Ic" int_sort [ gconst int_sort ]
    @ gblock
        "Ipred"
        bool_sort
        [ gterm (int_nonterm = int_nonterm)
        ; gterm (int_nonterm > int_nonterm)
        ; gterm (not bool_nonterm)
        ; gterm (bool_nonterm && bool_nonterm)
        ; gterm (bool_nonterm || bool_nonterm)
        ])
;;

let int_parametric ?(guess = None) (params : grammar_parameters) =
  let main_grammar =
    E.gblock
      "Ix"
      E.int_sort
      (E.[ gterm int_const_nonterm ]
      @ List.filter_map params.g_locals ~f:(fun (t, s) ->
            match s with
            | SId (_, IdSimple (_, "Int")) -> Some E.(gterm t)
            | _ -> None)
      @ E.[ gterm (neg int_nonterm) ]
      @ E.[ gterm (int_nonterm + int_nonterm) ]
      @ (if (not params.g_nonlinear) && params.g_mul_constant
        then
          E.
            [ gterm (int_const_nonterm * int_nonterm)
            ; gterm (int_nonterm / int_const_nonterm)
            ]
        else [])
      @ (if Set.mem params.g_opset (Binary Min)
        then E.[ gterm (min int_nonterm int_nonterm) ]
        else [])
      @ (if Set.mem params.g_opset (Binary Max)
        then E.[ gterm (max int_nonterm int_nonterm) ]
        else [])
      @ (if Set.mem params.g_opset (Binary Times)
            || Set.mem params.g_opset (Binary Div)
            || params.g_nonlinear
        then E.[ gterm (int_nonterm * int_nonterm); gterm (int_nonterm / int_nonterm) ]
        else [])
      @ (if Set.mem params.g_opset (Unary Abs) then E.[ gterm (abs int_nonterm) ] else [])
      @ (if Set.mem params.g_opset (Binary Div)
        then E.[ gterm (int_nonterm / int_const_nonterm) ]
        else [])
      @
      if params.g_bools
      then E.[ gterm (ite bool_nonterm int_nonterm int_nonterm) ]
      else [])
    @ E.(
        gblock
          "Ic"
          int_sort
          (if params.g_fixed_constants
          then [ gterm (int 0); gterm (int 1) ]
          else [ gconst int_sort ]))
    @
    if params.g_bools
    then
      E.gblock
        "Ipred"
        E.bool_sort
        (List.filter_map params.g_locals ~f:(fun (t, s) ->
             match s with
             | SId (_, IdSimple (_, "Bool")) -> Some E.(gterm t)
             | _ -> None)
        @ E.
            [ gterm (int_nonterm = int_nonterm)
            ; gterm (int_nonterm > int_nonterm)
            ; gterm (not bool_nonterm)
            ; gterm (bool_nonterm && bool_nonterm)
            ; gterm (bool_nonterm || bool_nonterm)
            ])
    else []
  in
  match guess with
  | None -> main_grammar
  | Some gguess ->
    preamble params E.int_sort ~ints:int_nonterm ~bools:bool_nonterm gguess @ main_grammar
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
        | SId (_, IdSimple (_, "Int")) -> true
        | _ -> false)
  in
  let bool_section =
    E.gblock
      "Ipred"
      E.bool_sort
      (List.filter_map params.g_locals ~f:(fun (t, s) ->
           match s with
           | SId (_, IdSimple (_, "Bool")) -> Some E.(gterm t)
           | _ -> None)
      @ E.
          [ gterm (not bool_nonterm)
          ; gterm (bool_nonterm && bool_nonterm)
          ; gterm (bool_nonterm || bool_nonterm)
          ]
      @ (if List.length params.g_locals <= 0
        then E.[ gterm mk_true; gterm mk_false ]
        else [])
      @
      if has_ints
      then
        (if special_const_prod then E.[ gterm (int_nonterm = int_const_nonterm) ] else [])
        @ E.
            [ gterm (int_nonterm = int_nonterm)
            ; gterm (int_nonterm > int_nonterm)
            ; gterm (int_nonterm >= int_nonterm)
            ]
      else [])
  in
  let int_section =
    E.gblock
      "Ix"
      E.int_sort
      ([ E.gterm int_const_nonterm ]
      @ List.filter_map params.g_locals ~f:(fun (t, s) ->
            match s with
            | SId (_, IdSimple (_, "Int")) -> Some (E.gterm t)
            | _ -> None)
      @ (if special_const_prod then E.[ gterm (int_nonterm + int_const_nonterm) ] else [])
      @ (if not short_predicate
        then
          E.[ gterm (neg int_nonterm) ]
          @ E.[ gterm (int_nonterm + int_nonterm) ]
          @ (if Set.mem params.g_opset (Binary Min)
            then E.[ gterm (min int_nonterm int_nonterm) ]
            else [])
          @
          if Set.mem params.g_opset (Binary Max)
          then E.[ gterm (max int_nonterm int_nonterm) ]
          else []
        else [])
      @ (if Set.mem params.g_opset (Binary Times)
        then E.[ gterm (int_const_nonterm * int_nonterm) ]
        else [])
      @ (if Set.mem params.g_opset (Binary Div)
        then E.[ gterm (int_nonterm / int_const_nonterm) ]
        else [])
      @ (if Set.mem params.g_opset (Unary Abs) then E.[ gterm (abs int_nonterm) ] else [])
      @ (if Set.mem params.g_opset (Binary Div)
        then E.[ gterm (int_nonterm / int_const_nonterm) ]
        else [])
      @ E.[ gterm (ite bool_nonterm int_nonterm int_nonterm) ]
      @ (if Set.mem params.g_opset (Binary Mod)
        then E.[ gterm (modulo int_nonterm int_nonterm) ]
        else [])
      @
      if Set.mem params.g_opset (Binary Times)
         || Set.mem params.g_opset (Binary Div)
         || params.g_nonlinear
      then E.[ gterm (int_nonterm * int_nonterm); gterm (int_nonterm / int_nonterm) ]
      else [])
    @ E.gblock
        "Ic"
        E.int_sort
        (if params.g_fixed_constants
        then E.[ gterm (int 0); gterm (int 1) ]
        else E.[ gconst int_sort ])
  in
  let main_grammar = if has_ints then bool_section @ int_section else bool_section in
  match guess with
  | None -> main_grammar
  | Some gguess ->
    preamble params E.bool_sort ~ints:int_nonterm ~bools:bool_nonterm gguess
    @ main_grammar
;;

let tuple_grammar_constr
    ~(ctx : Context.t)
    (params : grammar_parameters)
    (types : RType.t list)
  =
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
      E.gblock
        "Tr"
        (mk_sort_app
           (mk_id_simple "Tuple")
           (List.map ~f:(SygusInterface.sort_of_rtype ~ctx) types))
        [ E.gterm (mk_t_app (mk_id_simple "mkTuple") tuple_args) ]
    else (
      let tuple_type_name = Tuples.type_name_of_types types in
      let constr_name = Tuples.constr_name_of_types types in
      E.gblock
        "Tr"
        (E.sort tuple_type_name)
        [ E.gterm (mk_t_app (mk_id_simple constr_name) tuple_args) ])
  in
  head_rule @ int_parametric { params with g_bools = !use_bool }
;;

let grammar_sort_decomp (sort : sygus_sort) =
  match sort with
  | SId (_, IdSimple (_, "Int")) -> Some int_base
  | SId (_, IdSimple (_, "Bool")) -> None
  | SApp (_, IdSimple (_, "Tuple"), _) -> None
  | _ -> None
;;

let rec project ~(ctx : Context.t) (v : variable) =
  let rec aux =
    RType.(
      function
      | (TInt | TBool) as t -> [ E.var v.vname, sort_of_rtype ~ctx t ]
      | TParam _ -> [ E.var v.vname, E.int_sort ]
      | TTup types ->
        let l = List.map ~f:aux types in
        List.concat (List.mapi l ~f:(tuple_sel types))
      | _ as t -> [ E.var v.vname, sort_of_rtype ~ctx t ])
  in
  aux (Variable.vtype_or_new ctx v)

and tuple_sel types i projs =
  if !Config.using_cvc4_tuples
  then
    List.map projs ~f:(fun (t, s) ->
        mk_t_app (mk_id_indexed "tupSel" [ mk_index_num i ]) [ t ], s)
  else (
    let tuple_selector = Tuples.proj_name_of_types types i in
    List.map projs ~f:(fun (t, s) -> mk_t_app (mk_id_simple tuple_selector) [ t ], s))
;;

let generate_grammar
    ?(nonlinear = false)
    ?(guess = None)
    ?(bools = false)
    ?(special_const_prod = true)
    ?(short_predicate = false)
    ~(ctx : Context.t)
    (opset : OpSet.t)
    (args : variable list)
    (ret_sort : RType.t)
  =
  let locals_of_scalar_type = List.concat (List.map ~f:(project ~ctx) args) in
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
    | TTup types -> Some (tuple_grammar_constr ~ctx params types)
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
    ~(ctx : Context.t)
    (eqns : (term * term option * term * term) list)
    (xi : variable)
    : [> `First of string * variable list * term | `Second of Skeleton.t | `Third ] Lwt.t
  =
  match%lwt Deduction.Solver.presolve_equations ~orig_ctx:ctx ~xi eqns with
  | `Third -> Lwt.return (make_basic_guess eqns xi)
  | _ as l -> Lwt.return l
;;

let make_guess
    ?(level = 1)
    ~(ctx : Context.t)
    (xi : variable)
    (eqns : (term * term option * term * term) list)
    : [> `First of symbol * variable list * term | `Second of Skeleton.t | `Third ] Lwt.t
  =
  match level with
  | 0 -> Lwt.return `Third
  | 1 -> Lwt.return (make_basic_guess eqns xi)
  | 2 -> make_unification_guess ~ctx eqns xi
  | _ -> Lwt.return `Third
;;
