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
  | FTApp of term * term list
  | FTData of id * term list
  | FTVar of id
  | FTTup of term list
  | FTFun of term list * term
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


type pmrs_rule = loc * term * term

type pmrs_body = pmrs_rule list

type decl =
  | TypeDecl of loc * type_decl
  | FunDecl of loc * id * (id list) * term
  | PMRSDecl of loc * (id list) * id * (id list) * pmrs_body

type program = decl list

(*  Pretty printing *)
let rec pp_fterm (frmt : Formatter.t) (t : term) =
  let tkind = t.kind in
  match tkind with
  | FTConst c -> Constant.pp frmt c
  | FTApp (t1, t2) -> Fmt.(pf frmt "%a %a" pp_fterm t1 (list ~sep:sp pp_fterm) t2)
  | FTData (c, t2) -> Fmt.(pf frmt "%s(%a)" c (list ~sep:comma pp_fterm) t2)
  | FTVar v -> Fmt.string frmt v
  | FTTup l -> Fmt.(pf frmt "(%a)" (list ~sep:comma pp_fterm) l)
  | FTFun (args, body) -> Fmt.(pf frmt "(%a)->%a" (list ~sep:comma pp_fterm) args pp_fterm body)
  | FTBin (op, t1, t2) -> Fmt.(pf frmt "%a %a %a" pp_fterm t1 Binop.pp op pp_fterm t2)
  | FTUn (op, t1) -> Fmt.(pf frmt "%a %a" Unop.pp op pp_fterm t1)
  | FTHOBin op -> Fmt.(pf frmt "(%a)" Binop.pp op)
  | FTIte (c, a, b) -> Fmt.(pf frmt "(%a?%a:%a)" pp_fterm c pp_fterm a pp_fterm b)
