open Base
open Lang.Term
open Lang.RType
open Lexing

type loc = position * position

type id = string

type type_decl =
  | TDParametric of (id list) * id * type_term
  | TDSimple of id * type_term

type termkind =
  | FTConst of Constant.t
  | FTApp of term * term
  | FTData of id * term list
  | FTVar of id
  | FTTup of term list
  | FTFun of id list * term
  | FTBin of Binop.t * term * term
  | FTHOBin of Binop.t
  | FTUn of Unop.t * term
  | FTIte of term * term * term

and term =
  { pos : position * position; kind : termkind }

let mk_const pos c = {pos; kind = FTConst c}
let mk_app pos f args = {pos; kind = FTApp(f,args)}
let mk_data pos c args = {pos; kind = FTData(c, args)}
let mk_var pos v = {pos; kind = FTVar v}
let mk_bin pos op a b = {pos; kind = FTBin(op,a,b)}
let mk_un pos op a = {pos; kind=FTUn(op,a)}
let mk_ite pos c t f = {pos; kind=FTIte(c,t,f)}
let mk_tup pos tl = {pos; kind=FTTup tl}
let mk_fun pos args t = {pos; kind=FTFun(args,t)}


type pmrs_rule = loc * term list * term

type pmrs_body = pmrs_rule list

type pmrs_decl = loc * (id list) * id * (id list) * pmrs_body

type fun_decl = loc * id * (id list) * term

type decl =
  | TypeDecl of loc * type_decl
  | FunDecl of fun_decl
  | PMRSDecl of pmrs_decl

type program = decl list
