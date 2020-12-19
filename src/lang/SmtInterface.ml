open Base
open Term
open Smtlib.SmtLib
open Utils
open Option.Let_syntax



let string_of_smtSymbol (s : smtSymbol) : string =
  match s with
  | SSimple s -> s | SQuoted s -> "'"^s


let rec rtype_of_smtSort (s : smtSort) : RType.t option =
  match s with
  | SmtSort (Id sname) ->
    let%bind x = RType.get_type (string_of_smtSymbol sname) in
    (match x with
     | RType.TParam (_, maint) -> Some maint
     | _ -> Some x)

  | Comp (Id(sname), sort_params) ->
    let%bind x = RType.get_type (string_of_smtSymbol sname) in
    let%bind y = all_or_none (List.map ~f:rtype_of_smtSort sort_params) in
    (match x with
     | RType.TParam(params, maint) ->
       (match List.zip params y with
        | Ok l -> Some (RType.TParam (y, (RType.sub_all l maint)))
        | _ -> None )
     | _ -> None)

  | SmtSort (IdC (_, _)) ->  Log.error_msg "Indexed sorts not implemented."; None
  | Comp (IdC (_, _), _) ->  Log.error_msg "Indexed sorts not implemented."; None


let rec sort_of_rtype (t : RType.t) : smtSort =
  match t with
  | RType.TInt -> SmtSort (Id (SSimple "Int"))
  | RType.TBool -> SmtSort (Id (SSimple "Bool"))
  | RType.TString -> SmtSort (Id (SSimple "String"))
  | RType.TChar -> SmtSort (Id (SSimple "Char"))
  | RType.TNamed s -> SmtSort (Id (SSimple s))
  | RType.TTup tl -> Comp (Id (SSimple "Tuple"), List.map ~f:sort_of_rtype tl)
  | RType.TFun (tin, tout) -> Comp (Id (SSimple "->"), [sort_of_rtype tin; sort_of_rtype tout])
  | RType.TParam (args, t) -> dec_parametric t args
  | RType.TVar i -> SmtSort (Id (SSimple ("sort"^Int.to_string i)))

and dec_parametric t args =
  match t with
  | RType.TParam _ -> failwith "only one level of parameters supported in types."
  | RType.TNamed s -> Comp (Id (SSimple s), List.map ~f:sort_of_rtype args)
  | t -> sort_of_rtype t (* Not really parametric? *)



let term_of_const (c : Constant.t) : smtTerm =
  match c with
  | Constant.CInt i -> SmtTSpecConst (SCNumeral i)
  | Constant.CTrue -> mk_true
  | Constant.CFalse -> mk_false

let rec smt_of_term (t : term) : smtTerm =
  let tk = t.tkind in
  match tk with
  | TBin (op, t1, t2) -> mk_simple_app (Binop.to_string op) (List.map ~f:smt_of_term [t1;t2])
  | TUn (op, t1) -> mk_simple_app (Unop.to_string op) [smt_of_term t1]
  | TConst c -> term_of_const c
  | TVar x -> mk_var x.vname
  | TIte (c, a, b) -> mk_ite (smt_of_term c) (smt_of_term a) (smt_of_term b)
  | TTup tl -> mk_simple_app "mkTuple" (List.map ~f:smt_of_term tl)
  | TApp ({tkind=TVar v;_}, args) -> mk_simple_app v.vname  (List.map ~f:smt_of_term args)
  | TData (cstr, args) -> mk_simple_app cstr (List.map ~f:smt_of_term args)
  | TApp(_ , _) -> failwith "Smt: application function can only be variable."
  | TFun (_, _) -> failwith "Smt: functions in terms not supported."


let constant_of_smtConst (l : smtSpecConstant) : Constant.t =
  match l with
  | SCNumeral i -> Constant.CInt i
  | SCDecimal _ -> failwith "No reals in base language."
  | SCString _
  | SCBinary _
  | SCHexaDecimal _ -> failwith "No hex, bin or string constants in language."


type id_kind =
  | ICstr of string
  | IVar of variable
  | IBinop of Binop.t
  | IUnop of Unop.t
  | INotDef


let id_kind_of_s env s =
  match Map.find env s with
  | Some v -> IVar v
  | None ->
    match Binop.of_string s with
    | Some bop -> IBinop bop
    | None ->
      match Unop.of_string s with
      | Some unop -> IUnop unop
      | None -> match RType.type_of_variant s with
        | Some _ -> ICstr s
        | None -> INotDef


let rec term_of_sygus (env : (string, variable, String.comparator_witness) Map.t) (st : smtTerm) : term =
  match st with
  | SmtTQualdId (QI (Id (SSimple s))) ->
    (match Map.find env s with
     | Some v -> Term.mk_var v
     | None -> failwith "Variable not found.")

  | SmtTSpecConst l -> mk_const (constant_of_smtConst l)
  | SmtTApp (QI (Id (SSimple s)), args) ->
    let args' = List.map ~f:(term_of_sygus env) args in
    (match id_kind_of_s env s with
     | ICstr c -> mk_data c args'
     | IVar v -> mk_app (Term.mk_var v) args'
     | IBinop op ->
       (match args' with
        | [t1; t2] -> mk_bin op t1 t2
        | _ -> failwith "Sygus: a binary operator with more than two arguments.")
     | IUnop op ->
       (match args' with
        | [t1] -> mk_un op t1
        | _ -> failwith "Sygus: a unary operator with more than one argument.")
     | INotDef -> failwith "Sygus: Undefined variable.")
  | SmtTExists (_, _) -> failwith "Sygus: exists-terms not supported."
  | SmtTForall (_, _) -> failwith "Sygus: forall-terms not supported."
  | SmtTLet (_, _) -> failwith "Sygus: let-terms not supported."
  | _ -> failwith "Composite identifier not supported."


(* ============================================================================================= *)
(*                           COMMANDS                                                            *)
(* ============================================================================================= *)
include Smtlib.SmtLib

let decls_of_vars (vars : VarSet.t) =
  let f v =
    let sort = sort_of_rtype (Variable.vtype_or_new v) in
    DeclareConst (SSimple v.vname, sort)
  in List.map ~f (Set.elements vars)