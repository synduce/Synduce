open Base
open Lang.Term
open Syguslib.Sygus


type grammar_parameters =
  {
    g_opset : OpSet.t;
    g_fixed_constants : bool;
    g_mul_constant : bool;
    g_linear : bool;
    g_locals : (sygus_term * sygus_sort) list;
    g_bools : bool;
  }

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
  if Set.for_all opset ~f:Operator.is_lia then "DTLIA" else
    "DTNIA" (* TODO : for now only alternative is non-linear arithmetic *)


let int_sort = SId (IdSimple "Int")
let bool_sort = SId (IdSimple "Bool")

let int_base : grammar_def =
  let ic =  (SyId (IdSimple "Ic")) in
  let ix =  (SyId (IdSimple "Ix")) in
  let ipred = (SyId (IdSimple "Ipred")) in
  [("Ix", int_sort), [
      GTerm ic;
      GVar int_sort;
      GTerm (SyApp(IdSimple "-",[ix]));
      GTerm (SyApp(IdSimple "+",[ix]));
      GTerm (SyApp(IdSimple "min",[ix;ix]));
      GTerm (SyApp(IdSimple "max",[ix;ix]));
      GTerm (SyApp(IdSimple "*",[ic;ix]));
      GTerm (SyApp(IdSimple "div",[ix;ic]));
      GTerm (SyApp(IdSimple "abs",[ix]));
      GTerm (SyApp(IdSimple "div",[ix;ic]));
      GTerm (SyApp(IdSimple "ite",[ipred;ix;ix]));
    ];
   ("Ic", int_sort), [GConstant int_sort];
   ("Ipred", bool_sort), [
     GTerm (SyApp(IdSimple "=",[ix;ix]));
     GTerm (SyApp(IdSimple ">",[ix;ix]));
     GTerm (SyApp(IdSimple "not",[ipred]));
     GTerm (SyApp(IdSimple "and",[ipred;ipred]));
     GTerm (SyApp(IdSimple "or",[ipred;ipred]));
   ]]


let int_parametric (params : grammar_parameters) =
  let ic =  (SyId (IdSimple "Ic")) in
  let ix =  (SyId (IdSimple "Ix")) in
  let ipred = (SyId (IdSimple "Ipred")) in
  [("Ix", int_sort),
   [ GTerm ic] @
   (List.filter_map params.g_locals
      ~f:(fun (t,s) ->
          match s with SId (IdSimple "Int") -> Some (GTerm t) | _ -> None)) @
   [GTerm (SyApp(IdSimple "-",[ix]))] @
   [GTerm (SyApp(IdSimple "+",[ix; ix]))] @
   (if params.g_linear && params.g_mul_constant then
      [GTerm (SyApp(IdSimple "*",[ic;ix])); GTerm (SyApp(IdSimple "div",[ix;ic]))]
    else [] ) @
   (if Set.mem params.g_opset (Binary Min) then [GTerm (SyApp(IdSimple "min",[ix;ix]))] else []) @
   (if Set.mem params.g_opset (Binary Max) then [GTerm (SyApp(IdSimple "max",[ix;ix]))] else [] )@
   (if Set.mem params.g_opset (Binary Times) || Set.mem params.g_opset (Binary Div) || not params.g_linear
    then [GTerm (SyApp(IdSimple "*",[ix;ix])); GTerm (SyApp(IdSimple "div",[ix;ix]))]
    else [])@
   (if Set.mem params.g_opset (Unary Abs) then [GTerm (SyApp(IdSimple "abs",[ix]))] else [] )@
   (if Set.mem params.g_opset (Binary Div) then [GTerm (SyApp(IdSimple "div",[ix;ic]))] else [] ) @
   (if params.g_bools then [GTerm (SyApp(IdSimple "ite",[ipred;ix;ix]))] else [])
  ]
  @
  [
    ("Ic", int_sort),
    (if params.g_fixed_constants then
       [GTerm (SyLit (LitNum 0)); GTerm (SyLit (LitNum 1))]
     else [GConstant int_sort])
  ]
  @
  (if params.g_bools then
     [("Ipred", bool_sort),
      (List.filter_map params.g_locals
         ~f:(fun (t,s) ->
             match s with SId (IdSimple "Bool") -> Some (GTerm t) | _ -> None)) @
      [GTerm (SyApp(IdSimple "=",[ix;ix]));
       GTerm (SyApp(IdSimple ">",[ix;ix]));
       GTerm (SyApp(IdSimple "not",[ipred]));
       GTerm (SyApp(IdSimple "and",[ipred;ipred]));
       GTerm (SyApp(IdSimple "or",[ipred;ipred]))]]
   else [] )


let bool_parametric (params : grammar_parameters) =
  let has_ints =
    List.exists params.g_locals ~f:(fun (_, s) -> match s with SId (IdSimple "Int") -> true | _ -> false)
  in
  let ic =  (SyId (IdSimple "Ic")) in
  let ix =  (SyId (IdSimple "Ix")) in
  let ipred = (SyId (IdSimple "Ipred")) in
  let bool_section =
    [("Ipred", bool_sort),
     (List.filter_map params.g_locals
        ~f:(fun (t,s) ->
            match s with SId (IdSimple "Bool") -> Some (GTerm t) | _ -> None)) @
     [GTerm (SyApp(IdSimple "not",[ipred]));
      GTerm (SyApp(IdSimple "and",[ipred;ipred]));
      GTerm (SyApp(IdSimple "or",[ipred;ipred]))] @
     (if has_ints then
        [GTerm (SyApp(IdSimple "=",[ix;ix]));
         GTerm (SyApp(IdSimple ">",[ix;ix]))]
      else [])]
  in
  let int_section =
    [("Ix", int_sort),
     [ GTerm ic] @
     (List.filter_map params.g_locals
        ~f:(fun (t,s) ->
            match s with SId (IdSimple "Int") -> Some (GTerm t) | _ -> None)) @
     [GTerm (SyApp(IdSimple "-",[ix]))] @
     [GTerm (SyApp(IdSimple "+",[ix; ix]))] @
     (if Set.mem params.g_opset (Binary Min) then [GTerm (SyApp(IdSimple "min",[ix;ix]))] else []) @
     (if Set.mem params.g_opset (Binary Max) then [GTerm (SyApp(IdSimple "max",[ix;ix]))] else [] )@
     (if Set.mem params.g_opset (Binary Times) then [GTerm (SyApp(IdSimple "*",[ic;ix]))] else [] )@
     (if Set.mem params.g_opset (Binary Div) then [GTerm (SyApp(IdSimple "div",[ix;ic]))] else [] )@
     (if Set.mem params.g_opset (Unary Abs) then [GTerm (SyApp(IdSimple "abs",[ix]))] else [] )@
     (if Set.mem params.g_opset (Binary Div) then [GTerm (SyApp(IdSimple "div",[ix;ic]))] else [] ) @
     [GTerm (SyApp(IdSimple "ite",[ipred;ix;ix]))]
    ]
    @
    [
      ("Ic", int_sort),
      (if params.g_fixed_constants then
         [GTerm (SyLit (LitNum 0)); GTerm (SyLit (LitNum 1))]
       else [GConstant int_sort])
    ]
  in
  if has_ints then bool_section @ int_section else bool_section


let tuple_grammar_constr (params : grammar_parameters) (sorts : sygus_sort list) =
  let tuple_args =
    List.map sorts
      ~f:(function
          | SId (IdSimple "Int") ->  SyId (IdSimple "Ix")
          | SId (IdSimple "Bool") -> SyId (IdSimple "Ipred")
          | _ -> failwith "TODO: complex tuples not supported.")
  in
  let head_rule =
    ("Tr", SApp(IdSimple "Tuple", sorts)), [GTerm (SyApp (IdSimple "mkTuple", tuple_args))]
  in
  head_rule :: (int_parametric params)



let grammar_sort_decomp (sort : sygus_sort) =
  match sort with
  | SId (IdSimple "Int") -> Some int_base
  | SId (IdSimple "Bool") -> None
  | SApp (IdSimple "Tuple", _) -> None
  |  _ -> None


let rec project (v, s : sorted_var) =
  match s with
  | SId (IdSimple _) -> [SyId (IdSimple v), s]

  | SApp(IdSimple "Tuple", sorts) ->
    let l = List.map ~f:project (List.map ~f:(fun s -> (v,s)) sorts) in
    List.concat (List.mapi l ~f:tuple_sel)

  | _ -> [SyId (IdSimple v), s]

and tuple_sel i projs =
  List.map projs ~f:(fun (t, s) -> SyApp(IdIndexed("tupSel", [INum i]), [t]), s)


let generate_grammar (opset : OpSet.t) (args : sorted_var list) (ret_sort : sygus_sort) =
  let locals_of_scalar_type = List.concat (List.map ~f:project args) in
  let params =
    {
      g_opset = opset;
      g_fixed_constants = true;
      g_mul_constant = false;
      g_linear = true;
      g_locals = locals_of_scalar_type;
      g_bools = false;
    }
  in
  match ret_sort with
  | SId (IdSimple "Int") -> Some (int_parametric params)
  | SId (IdSimple "Bool") -> Some (bool_parametric params)
  | SApp(IdSimple "Tuple", sorts) -> Some (tuple_grammar_constr params sorts)
  | _ -> None