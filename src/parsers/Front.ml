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
  | FTLet of term * term * term
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

let mk_let pos v e body = {pos;kind=FTLet(v, e, body)}

type pmrs_rule = loc * term * term

type pmrs_body = pmrs_rule list

type function_body =
  | PmrsBody of loc * pmrs_body
  | ExprBody of loc * term

type decl =
  | TypeDecl of loc * type_decl
  | FunDecl of loc * id * (id list) * term option * term
  | PMRSDecl of loc * (id list) * id * (id list) * term option * pmrs_body
  | CamlPMRSDecl of loc * (id list) * term option * (id * (id list) * function_body) list
  | SyntObjDecl of loc * decl * id * id



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
  | FTLet (x, e, body) -> Fmt.(pf frmt "let %a = @[<hov 2>%a@]@;in@;@[<hov 2>%a@]"
                                 pp_fterm x pp_fterm e pp_fterm body)
  | FTFun (args, body) -> Fmt.(pf frmt "(%a)->%a" (list ~sep:comma pp_fterm) args pp_fterm body)
  | FTBin (op, t1, t2) -> Fmt.(pf frmt "%a %a %a" pp_fterm t1 Binop.pp op pp_fterm t2)
  | FTUn (op, t1) -> Fmt.(pf frmt "%a %a" Unop.pp op pp_fterm t1)
  | FTHOBin op -> Fmt.(pf frmt "(%a)" Binop.pp op)
  | FTIte (c, a, b) -> Fmt.(pf frmt "(%a?%a:%a)" pp_fterm c pp_fterm a pp_fterm b)

(* Preprocessing progam *)
let make_rules functions =
  let f (name, args, func_body) =
    match func_body with
    | PmrsBody (ploc, rules) ->
      let f (loc, lhs, rhs) =
        let fargs = List.map ~f:(fun x -> mk_var loc x) args in
        loc, mk_app ploc (mk_var ploc name) (fargs @ [lhs]), rhs
      in
      List.map ~f rules
    | ExprBody (loc, term) ->
      [loc, mk_app loc (mk_var loc name) [mk_var loc (List.last_exn args)], term]
  in
  List.concat (List.map ~f functions)


let rebuild_pmrs_decl loc params inv functions =
  match functions with
  | (name, args, fbody) :: _ ->
    Utils.Log.verbose_msg ("Preprocessing "^name);
      let parametric_args =
        match fbody with
        | PmrsBody _ -> args
        | ExprBody _ ->
          match List.drop_last args with
          | Some pargs -> pargs
          | None ->
            Utils.Log.error_msg Fmt.(str "%s should have at least one argument." name);
            failwith "Not a proper recursion scheme."
      in
      PMRSDecl(loc, params, name, parametric_args, inv, make_rules functions)
  | [] -> PMRSDecl(loc, params, "??", [], inv, [])


let preprocess (prog : program) : program * (ident * ident * ident) option =
    let obj_name = ref None in
    let rec f d =
      match d with
      | CamlPMRSDecl(loc, params, invariant, functions) ->
        rebuild_pmrs_decl loc params invariant functions
      | SyntObjDecl(_, target_pmrs, spec_id, repr_id) ->
        let rskel_name, d =
          match f target_pmrs with
          | PMRSDecl(_, _, name, _, _, _) as d ->
            Utils.Log.verbose_msg Fmt.(str "Synthesis objective %s = %s %s" name spec_id repr_id);
              Some name, d
          | d -> None, d
        in
        (match rskel_name with
        | Some n -> obj_name := Some (n, spec_id, repr_id)
        | None -> ());
        d
      | _ -> d
    in
    let processed_decls = List.map ~f prog in
    processed_decls, !obj_name