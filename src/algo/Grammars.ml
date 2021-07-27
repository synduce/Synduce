open Base
open Lang.Term
open Syguslib.Sygus

type grammar_parameters = {
  g_opset : OpSet.t;
  g_fixed_constants : bool;
  g_mul_constant : bool;
  g_linear : bool;
  g_locals : (sygus_term * sygus_sort) list;
  g_bools : bool;
}

type grammar_guess = GBin of Binop.t | GUn of Unop.t | GIte | GNonGuessable

let operators_of (t : term) : OpSet.t =
  let init = OpSet.empty in
  let join = Set.union in
  let case f x =
    match x.tkind with
    | TBin (op, t1, t2) -> Some (join (OpSet.singleton (Binary op)) (join (f t1) (f t2)))
    | TUn (op, t1) -> Some (join (OpSet.singleton (Unary op)) (f t1))
    | _ -> None
  in
  reduce ~init ~join ~case t

let logic_of_operator (opset : OpSet.t) : string =
  if Set.for_all opset ~f:Operator.is_lia then "DTLIA" else "DTNIA"

(* TODO : for now only alternative is non-linear arithmetic *)

let int_sort = SId (IdSimple "Int")

let bool_sort = SId (IdSimple "Bool")

let int_base : grammar_def =
  let ic = SyId (IdSimple "Ic") in
  let ix = SyId (IdSimple "Ix") in
  let ipred = SyId (IdSimple "Ipred") in
  [
    ( ("Ix", int_sort),
      [
        GTerm ic;
        GVar int_sort;
        GTerm (SyApp (IdSimple "-", [ ix ]));
        GTerm (SyApp (IdSimple "+", [ ix ]));
        GTerm (SyApp (IdSimple "min", [ ix; ix ]));
        GTerm (SyApp (IdSimple "max", [ ix; ix ]));
        GTerm (SyApp (IdSimple "*", [ ic; ix ]));
        GTerm (SyApp (IdSimple "div", [ ix; ic ]));
        GTerm (SyApp (IdSimple "abs", [ ix ]));
        GTerm (SyApp (IdSimple "div", [ ix; ic ]));
        GTerm (SyApp (IdSimple "ite", [ ipred; ix; ix ]));
      ] );
    (("Ic", int_sort), [ GConstant int_sort ]);
    ( ("Ipred", bool_sort),
      [
        GTerm (SyApp (IdSimple "=", [ ix; ix ]));
        GTerm (SyApp (IdSimple ">", [ ix; ix ]));
        GTerm (SyApp (IdSimple "not", [ ipred ]));
        GTerm (SyApp (IdSimple "and", [ ipred; ipred ]));
        GTerm (SyApp (IdSimple "or", [ ipred; ipred ]));
      ] );
  ]

let int_parametric ?(guess = None) (params : grammar_parameters) =
  let ic = SyId (IdSimple "Ic") in
  let ix = SyId (IdSimple "Ix") in
  let ipred = SyId (IdSimple "Ipred") in
  let main_grammar =
    [
      ( ("Ix", int_sort),
        [ GTerm ic ]
        @ List.filter_map params.g_locals ~f:(fun (t, s) ->
              match s with SId (IdSimple "Int") -> Some (GTerm t) | _ -> None)
        @ [ GTerm (SyApp (IdSimple "-", [ ix ])) ]
        @ [ GTerm (SyApp (IdSimple "+", [ ix; ix ])) ]
        @ (if params.g_linear && params.g_mul_constant then
           [ GTerm (SyApp (IdSimple "*", [ ic; ix ])); GTerm (SyApp (IdSimple "div", [ ix; ic ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Min) then
           [ GTerm (SyApp (IdSimple "min", [ ix; ix ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Max) then
           [ GTerm (SyApp (IdSimple "max", [ ix; ix ])) ]
          else [])
        @ (if
           Set.mem params.g_opset (Binary Times)
           || Set.mem params.g_opset (Binary Div)
           || not params.g_linear
          then
           [ GTerm (SyApp (IdSimple "*", [ ix; ix ])); GTerm (SyApp (IdSimple "div", [ ix; ix ])) ]
          else [])
        @ (if Set.mem params.g_opset (Unary Abs) then [ GTerm (SyApp (IdSimple "abs", [ ix ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Div) then
           [ GTerm (SyApp (IdSimple "div", [ ix; ic ])) ]
          else [])
        @ if params.g_bools then [ GTerm (SyApp (IdSimple "ite", [ ipred; ix; ix ])) ] else [] );
    ]
    @ [
        ( ("Ic", int_sort),
          if params.g_fixed_constants then [ GTerm (SyLit (LitNum 0)); GTerm (SyLit (LitNum 1)) ]
          else [ GConstant int_sort ] );
      ]
    @
    if params.g_bools then
      [
        ( ("Ipred", bool_sort),
          List.filter_map params.g_locals ~f:(fun (t, s) ->
              match s with SId (IdSimple "Bool") -> Some (GTerm t) | _ -> None)
          @ [
              GTerm (SyApp (IdSimple "=", [ ix; ix ]));
              GTerm (SyApp (IdSimple ">", [ ix; ix ]));
              GTerm (SyApp (IdSimple "not", [ ipred ]));
              GTerm (SyApp (IdSimple "and", [ ipred; ipred ]));
              GTerm (SyApp (IdSimple "or", [ ipred; ipred ]));
            ] );
      ]
    else []
  in
  match guess with
  | None -> main_grammar
  | Some gguess ->
      let preamble =
        match gguess with
        | GUn u ->
            [ (("IStart", int_sort), [ GTerm (SyApp (IdSimple (Unop.to_string u), [ ix ])) ]) ]
        | GBin b -> (
            let args =
              match Binop.operand_types b with
              | (Lang.RType.TInt, Lang.RType.TInt) :: _ -> Some [ ix; ix ]
              | [ (Lang.RType.TInt, Lang.RType.TBool) ] -> Some [ ix; ipred ]
              | [ (Lang.RType.TBool, Lang.RType.TBool) ] -> Some [ ipred; ipred ]
              | _ -> None
            in
            match args with
            | Some args ->
                Utils.Log.debug_msg
                  Fmt.(str "Grammar optimization: top binary symbol is %s" (Binop.to_string b));
                [ (("IStart", int_sort), [ GTerm (SyApp (IdSimple (Binop.to_string b), args)) ]) ]
            | None -> [])
        | GIte -> [ (("IStart", int_sort), [ GTerm (SyApp (IdSimple "ite", [ ipred; ix; ix ])) ]) ]
        | _ -> []
      in
      preamble @ main_grammar

let bool_parametric ?(guess = None) (params : grammar_parameters) =
  let has_ints =
    List.exists params.g_locals ~f:(fun (_, s) ->
        match s with SId (IdSimple "Int") -> true | _ -> false)
  in
  let ic = SyId (IdSimple "Ic") in
  let ix = SyId (IdSimple "Ix") in
  let ipred = SyId (IdSimple "Ipred") in
  let bool_section =
    [
      ( ("Ipred", bool_sort),
        List.filter_map params.g_locals ~f:(fun (t, s) ->
            match s with SId (IdSimple "Bool") -> Some (GTerm t) | _ -> None)
        @ [
            GTerm (SyApp (IdSimple "not", [ ipred ]));
            GTerm (SyApp (IdSimple "and", [ ipred; ipred ]));
            GTerm (SyApp (IdSimple "or", [ ipred; ipred ]));
          ]
        @
        if has_ints then
          [
            GTerm (SyApp (IdSimple "=", [ ix; ic ]));
            GTerm (SyApp (IdSimple "=", [ ix; ix ]));
            GTerm (SyApp (IdSimple ">", [ ix; ix ]));
          ]
        else [] );
    ]
  in
  let int_section =
    [
      ( ("Ix", int_sort),
        [ GTerm ic ]
        @ List.filter_map params.g_locals ~f:(fun (t, s) ->
              match s with SId (IdSimple "Int") -> Some (GTerm t) | _ -> None)
        @ [ GTerm (SyApp (IdSimple "-", [ ix ])) ]
        @ [ GTerm (SyApp (IdSimple "+", [ ix; ix ])) ]
        @ [ GTerm (SyApp (IdSimple "+", [ ix; ic ])) ]
        @ (if Set.mem params.g_opset (Binary Min) then
           [ GTerm (SyApp (IdSimple "min", [ ix; ix ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Max) then
           [ GTerm (SyApp (IdSimple "max", [ ix; ix ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Times) then
           [ GTerm (SyApp (IdSimple "*", [ ic; ix ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Div) then
           [ GTerm (SyApp (IdSimple "div", [ ix; ic ])) ]
          else [])
        @ (if Set.mem params.g_opset (Unary Abs) then [ GTerm (SyApp (IdSimple "abs", [ ix ])) ]
          else [])
        @ (if Set.mem params.g_opset (Binary Div) then
           [ GTerm (SyApp (IdSimple "div", [ ix; ic ])) ]
          else [])
        @ [ GTerm (SyApp (IdSimple "ite", [ ipred; ix; ix ])) ]
        @
        if Set.mem params.g_opset (Binary Mod) then [ GTerm (SyApp (IdSimple "mod", [ ix; ix ])) ]
        else [] );
    ]
    @ [
        ( ("Ic", int_sort),
          if params.g_fixed_constants then [ GTerm (SyLit (LitNum 0)); GTerm (SyLit (LitNum 1)) ]
          else [ GConstant int_sort ] );
      ]
  in
  let main_grammar = if has_ints then bool_section @ int_section else bool_section in
  match guess with
  | None -> main_grammar
  | Some gguess ->
      let preamble =
        match gguess with
        | GUn u ->
            [ (("IStart", bool_sort), [ GTerm (SyApp (IdSimple (Unop.to_string u), [ ix ])) ]) ]
        | GBin b -> (
            let args =
              match Binop.operand_types b with
              | (Lang.RType.TInt, Lang.RType.TInt) :: _ -> Some [ ix; ix ]
              | [ (Lang.RType.TInt, Lang.RType.TBool) ] -> Some [ ix; ipred ]
              | [ (Lang.RType.TBool, Lang.RType.TBool) ] -> Some [ ipred; ipred ]
              | _ -> None
            in
            match args with
            | Some args ->
                [ (("IStart", bool_sort), [ GTerm (SyApp (IdSimple (Binop.to_string b), args)) ]) ]
            | None -> [])
        | GIte ->
            [ (("IStart", bool_sort), [ GTerm (SyApp (IdSimple "ite", [ ipred; ipred; ipred ])) ]) ]
        | _ -> []
      in
      preamble @ main_grammar

let tuple_grammar_constr (params : grammar_parameters) (sorts : sygus_sort list) =
  let use_bool = ref params.g_bools in
  let tuple_args =
    List.map sorts ~f:(function
      | SId (IdSimple "Int") -> SyId (IdSimple "Ix")
      | SId (IdSimple "Bool") ->
          use_bool := true;
          SyId (IdSimple "Ipred")
      | _ -> failwith "TODO: complex tuples not supported.")
  in
  let head_rule =
    (("Tr", SApp (IdSimple "Tuple", sorts)), [ GTerm (SyApp (IdSimple "mkTuple", tuple_args)) ])
  in
  head_rule :: int_parametric { params with g_bools = !use_bool }

let grammar_sort_decomp (sort : sygus_sort) =
  match sort with
  | SId (IdSimple "Int") -> Some int_base
  | SId (IdSimple "Bool") -> None
  | SApp (IdSimple "Tuple", _) -> None
  | _ -> None

let rec project ((v, s) : sorted_var) =
  match s with
  | SId (IdSimple _) -> [ (SyId (IdSimple v), s) ]
  | SApp (IdSimple "Tuple", sorts) ->
      let l = List.map ~f:project (List.map ~f:(fun s -> (v, s)) sorts) in
      List.concat (List.mapi l ~f:tuple_sel)
  | _ -> [ (SyId (IdSimple v), s) ]

and tuple_sel i projs =
  List.map projs ~f:(fun (t, s) -> (SyApp (IdIndexed ("tupSel", [ INum i ]), [ t ]), s))

let generate_grammar ?(guess = None) ?(bools = false) (opset : OpSet.t) (args : sorted_var list)
    (ret_sort : sygus_sort) =
  let locals_of_scalar_type = List.concat (List.map ~f:project args) in
  let params =
    {
      g_opset = opset;
      g_fixed_constants = false;
      g_mul_constant = false;
      g_linear = true;
      g_locals = locals_of_scalar_type;
      g_bools = bools || not (Set.are_disjoint opset OpSet.comparison_operators);
    }
  in
  match ret_sort with
  | SId (IdSimple "Int") -> Some (int_parametric ~guess params)
  | SId (IdSimple "Bool") -> Some (bool_parametric ~guess params)
  (* Ignore guess if it's a tuple that needs to be generated. *)
  | SApp (IdSimple "Tuple", sorts) -> Some (tuple_grammar_constr params sorts)
  | _ -> None

let make_guess (eqns : (term * term option * term * term) list) (xi : variable) =
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
      match lhs.tkind with
      | TBin (op, _, _) -> Some (GBin op)
      | TUn (op, _) -> Some (GUn op)
      | TIte (_, _, _) -> Some GIte
      | TConst _ -> None
      | TVar _ -> None
      | _ -> Some GNonGuessable
    in
    List.filter_map ~f lhs_of_xi
  in
  match guesses with
  | [] -> None
  | hd :: tl -> if List.for_all tl ~f:(fun x -> Poly.equal x hd) then Some hd else None
