open Lang.Term
open Lexing

type id = string


type type_term =
  | TInt
  | TBool
  | TTyp of id
  | TParam of id
  | TParamTyp of (type_term list) * type_term
  | TSum of type_term list
  | TConstr of id * (type_term list)

type type_decl =
  | TDParametric of (id list) * id * type_term
  | TDSimple of id * type_term

type termkind =
  | TConst of Constant.t
  | TApp of term * term
  | TData of id * term list
  | TVar of id
  | TBin of Binop.t * term * term
  | THOBin of Binop.t
  | TUn of Unop.t * term
  | TIte of term * term * term

and term =
  { pos : position * position; kind : termkind }

let mk_const pos c = {pos; kind = TConst c}
let mk_app pos f args = {pos; kind = TApp(f,args)}
let mk_data pos c args = {pos; kind = TData(c, args)}
let mk_var pos v = {pos; kind = TVar v}
let mk_bin pos op a b = {pos; kind = TBin(op,a,b)}
let mk_un pos op a = {pos; kind=TUn(op,a)}
let mk_ite pos c t f = {pos; kind=TIte(c,t,f)}


type pmrs_rule = term list * term

type pmrs_body = pmrs_rule list

type pmrs_decl = (id list) * id * (id list) * pmrs_body

type fun_decl = id * (id list) * term

type decl =
  | TypeDecl of type_decl
  | FunDecl of fun_decl
  | PMRSDecl of pmrs_decl

type program = decl list