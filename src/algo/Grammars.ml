open Base
open Lang.Term
open Syguslib.Sygus


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

let int_parametric
    ?(fixed_constants = true)
    ?(boolean_predicates = false)
    (opset : OpSet.t) (_ : sorted_var list) =
  let ic =  (SyId (IdSimple "Ic")) in
  let ix =  (SyId (IdSimple "Ix")) in
  let ipred = (SyId (IdSimple "Ipred")) in
  [("Ix", int_sort),
   [ GTerm ic; GVar int_sort] @
   [GTerm (SyApp(IdSimple "-",[ix]))] @
   [GTerm (SyApp(IdSimple "+",[ix; ix]))] @
   (if Set.mem opset (Binary Min) then [GTerm (SyApp(IdSimple "min",[ix;ix]))] else []) @
   (if Set.mem opset (Binary Max) then [GTerm (SyApp(IdSimple "max",[ix;ix]))] else [] )@
   (if Set.mem opset (Binary Times) then [GTerm (SyApp(IdSimple "*",[ic;ix]))] else [] )@
   (if Set.mem opset (Binary Div) then [GTerm (SyApp(IdSimple "div",[ix;ic]))] else [] )@
   (if Set.mem opset (Unary Abs) then [GTerm (SyApp(IdSimple "abs",[ix]))] else [] )@
   (if Set.mem opset (Binary Div) then [GTerm (SyApp(IdSimple "div",[ix;ic]))] else [] ) @
   (if boolean_predicates then [GTerm (SyApp(IdSimple "ite",[ipred;ix;ix]))] else [])
  ]
  @
  [
    ("Ic", int_sort),
    (if fixed_constants then
       [GTerm (SyLit (LitNum 0)); GTerm (SyLit (LitNum 1))]
     else [GConstant int_sort])
  ]
  @
  (if boolean_predicates then
     [("Ipred", bool_sort), [
         GVar bool_sort;
         GTerm (SyApp(IdSimple "=",[ix;ix]));
         GTerm (SyApp(IdSimple ">",[ix;ix]));
         GTerm (SyApp(IdSimple "not",[ipred]));
         GTerm (SyApp(IdSimple "and",[ipred;ipred]));
         GTerm (SyApp(IdSimple "or",[ipred;ipred]))]]
   else [] )

let bool_base_default =
  ["Ix", bool_sort; "Ipred", int_sort]

let grammar_sort_decomp (sort : sygus_sort) =
  match sort with
  | SId (IdSimple "Int") -> Some int_base
  | SId (IdSimple "Bool") -> None
  | SApp (IdSimple "Tuple", _) -> None
  |  _ -> None

let generate_grammar (opset : OpSet.t) (locals : sorted_var list) (ret_sort : sygus_sort) =
  match ret_sort with
  | SId (IdSimple "Int") -> Some (int_parametric opset locals)
  | _ -> None