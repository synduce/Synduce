open Base
open Syguslib.Sygus
open Term
open Option.Let_syntax
open Utils


let rec rtype_of_sort (s : sygus_sort) : RType.t option =
  match s with
  | SId (IdSimple sname) ->
    let%bind x = RType.get_type sname in
    (match x with
     | RType.TParam (_, maint) -> Some maint
     | _ -> Some x)

  | SApp (IdSimple("Tuple"), sorts) ->
    (match all_or_none (List.map ~f:rtype_of_sort sorts) with
     | Some l -> Some (RType.TTup l)
     | _ -> None)

  | SApp (IdSimple(sname), sort_params) ->
    let%bind x = RType.get_type sname in
    let%bind y = all_or_none (List.map ~f:rtype_of_sort sort_params) in
    (match x with
     | RType.TParam(params, maint) ->
       (match List.zip params y with
        | Ok l -> Some (RType.TParam (y, (RType.sub_all l maint)))
        | _ -> None )
     | _ -> None)

  | SId (_) ->  Log.error_msg "Indexed / qualified sorts not implemented."; None
  | SApp (_, _) ->  Log.error_msg "Indexed sorts not implemented."; None


let rec sort_of_rtype (t : RType.t) : sygus_sort =
  match t with
  | RType.TInt -> SId (IdSimple "Int")
  | RType.TBool -> SId (IdSimple "Bool")
  | RType.TString -> SId (IdSimple "String")
  | RType.TChar -> SId (IdSimple "Char")
  | RType.TNamed s -> SId (IdSimple s)
  | RType.TTup tl -> SApp (IdSimple "Tuple", List.map ~f:sort_of_rtype tl)
  | RType.TFun (tin, tout) -> SApp(IdSimple "->", [sort_of_rtype tin; sort_of_rtype tout])
  | RType.TParam (args, t) -> dec_parametric t args
  | RType.TVar _ -> SId (IdSimple ("Int")) (* TODO: declare sort? *)

and dec_parametric t args =
  match t with
  | RType.TParam _ -> failwith "only one level of parameters supported in types."
  | RType.TNamed s -> SApp(IdSimple s, List.map ~f:sort_of_rtype args)
  | t -> sort_of_rtype t (* Not really parametric? *)



let literal_of_const (c : Constant.t) : literal =
  match c with
  | Constant.CInt i -> LitNum i
  | Constant.CTrue -> LitBool true
  | Constant.CFalse -> LitBool false

let rec sygus_of_term (t : term) : sygus_term =
  let tk = t.tkind in
  match tk with
  | TBin (op, t1, t2) -> SyApp (IdSimple (Binop.to_string op), List.map ~f:sygus_of_term [t1;t2])
  | TUn (op, t1) -> SyApp (IdSimple (Unop.to_string op), [sygus_of_term t1])
  | TConst c -> SyLit (literal_of_const c)
  | TVar x -> SyId (IdSimple x.vname)
  | TIte (c, a, b) -> SyApp( IdSimple "ite", List.map ~f:sygus_of_term [c;a;b])
  | TTup tl -> SyApp(IdSimple "mkTuple", List.map ~f:sygus_of_term tl)
  | TSel (t,i) -> SyApp(IdIndexed("tupleSel",[INum i]), [sygus_of_term t])
  | TApp ({tkind=TVar v;_}, args) -> SyApp(IdSimple v.vname, List.map ~f:sygus_of_term args)
  | TData (cstr, args) -> SyApp(IdSimple cstr, List.map ~f:sygus_of_term args)
  | TApp(_ , _) -> failwith "Sygus: application function can only be variable."
  | TFun (_, _) -> failwith "Sygus: functions in terms not supported."


let constant_of_literal (l : literal) : Constant.t =
  match l with
  | LitNum i -> Constant.CInt i
  | LitBool b -> if b then Constant.CTrue else Constant.CFalse
  | LitDec _ -> failwith "No reals in base language."
  | LitHex _
  | LitBin _
  | LitString _ -> failwith "No hex, bin or string constants in language."


type id_kind =
  | ICstr of string
  | IVar of variable
  | IBinop of Binop.t
  | IUnop of Unop.t
  | ITupleAccessor of int
  | INotDef
  | ITupleCstr
  | IIte


let id_kind_of_s env s =
  let string_case s =
    match s with
    | "ite" ->  IIte
    | "mkTuple" -> ITupleCstr
    | s when String.is_prefix ~prefix:"__cvc4_tuple_" s ->
      let i = Int.of_string (Str.last_chars s 1) in
      ITupleAccessor i
    | _ -> INotDef
  in
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
        | None -> string_case s


let rec term_of_sygus (env : (string, variable, String.comparator_witness) Map.t) (st : sygus_term) : term =
  match st with
  | SyId (IdSimple s) ->
    (match Map.find env s with
     | Some v -> mk_var v
     | None -> failwith "Variable not found.")

  | SyLit l -> mk_const (constant_of_literal l)
  | SyApp (IdSimple s, args) ->
    let args' = List.map ~f:(term_of_sygus env) args in
    (match id_kind_of_s env s with
     | ICstr c -> mk_data c args'
     | IVar v -> mk_app (mk_var v) args'
     | IBinop op ->
       (match args' with
        | [t1; t2] -> mk_bin op t1 t2
        | _ -> failwith "Sygus: a binary operator with more than two arguments.")
     | IUnop op ->
       (match args' with
        | [t1] -> mk_un op t1
        | _ -> failwith "Sygus: a unary operator with more than one argument.")
     | IIte ->
       (match args' with
        | [t1; t2; t3] -> mk_ite t1 t2 t3
        | _ -> failwith "Sygus: a binary operator with more than two arguments.")
     | ITupleAccessor i ->
       (match args' with
        | [arg] -> mk_sel arg i
        | _ -> failwith "Sygus: a tuple acessor with wrong number of arguments")
     | ITupleCstr -> mk_tup args'
     | INotDef -> failwith Fmt.(str "Sygus: Undefined variable %s" s))
  | SyExists (_, _) -> failwith "Sygus: exists-terms not supported."
  | SyForall (_, _) -> failwith "Sygus: forall-terms not supported."
  | SyLet (_, _) -> failwith "Sygus: let-terms not supported."
  | _ -> failwith "Composite identifier not supported."


(* ============================================================================================= *)
(*                           COMMANDS                                                            *)
(* ============================================================================================= *)

let declaration_of_var (v : variable) =  CDeclareVar(v.vname, sort_of_rtype (Variable.vtype_or_new v))

let sorted_vars_of_types (tl : RType.t list) : sorted_var list =
  let f t =
    (* Declare var for future parsing. *)
    let varname = Alpha.fresh "x" in
    varname, sort_of_rtype t
  in
  List.map ~f tl