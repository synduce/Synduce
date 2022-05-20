open Front
open Base
open Parsetree
open Utils
module T = Lang.Term
module Typ = Lang.RType

let synt_extension = "synt"

(* Synthesis objectives *)
let _synt_objects : (string, int) Hashtbl.t = Hashtbl.create (module String)
let reset_synt_objects () = Hashtbl.clear _synt_objects
let _modules : (string, Typ.t) Hashtbl.t = Hashtbl.create (module String)

let add_synt_payload s =
  match Hashtbl.find _synt_objects s with
  | Some x -> Hashtbl.set _synt_objects ~key:s ~data:(x + 1)
  | None -> Hashtbl.add_exn _synt_objects ~key:s ~data:0
;;

let get_objects () = Hashtbl.to_alist _synt_objects
let add_module (s : string) (mt : Typ.t) = Hashtbl.set _modules ~key:s ~data:mt
let get_module (s : string) = Hashtbl.find _modules s

(* ============================================================================================= *)

let sloc (locator : 'a Location.loc) : Lexing.position * Lexing.position =
  locator.loc.loc_start, locator.loc.loc_end
;;

let wloc (locator : Warnings.loc) : Lexing.position * Lexing.position =
  locator.loc_start, locator.loc_end
;;

let simple_ident_of_longident (l : Longident.t) : string option =
  match l with
  | Longident.Lident i -> Some i
  | _ -> None
;;

let simple_ident_of_expr (expr : expression) =
  match expr.pexp_desc with
  | Pexp_ident loc -> simple_ident_of_longident loc.txt
  | _ -> None
;;

let tuple_of_idents_expr (expr : expression) : string list option =
  match expr.pexp_desc with
  | Pexp_tuple exprs -> all_or_none (List.map ~f:simple_ident_of_expr exprs)
  | _ -> None
;;

let fconst_of_constant (const : constant) : T.Constant.t =
  match const with
  | Pconst_integer (value, _) -> T.Constant.of_int (Int.of_string value)
  | _ -> failwith "Unsupported constant."
;;

let rec fterm_of_expr (expr : expression) : term =
  let cur_loc = wloc expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_ident locid ->
    (match Longident.flatten locid.txt with
    | [ id ] -> mk_var (sloc locid) id
    | [ maybe_module_name; id ] ->
      (match get_module maybe_module_name, id with
      | Some (TSet t), "empty" -> mk_const cur_loc (T.Constant.CEmptySet t)
      | _ ->
        failwith
          (Fmt.str
             "Longident %s not supported."
             (String.concat ~sep:"." (Longident.flatten locid.txt))))
    | _ ->
      failwith
        (Fmt.str
           "Longident %s not supported."
           (String.concat ~sep:"." (Longident.flatten locid.txt))))
  | Pexp_constant const -> mk_const cur_loc (fconst_of_constant const)
  | Pexp_ifthenelse (c, tt, otf) ->
    (match otf with
    | Some tf -> mk_ite cur_loc (fterm_of_expr c) (fterm_of_expr tt) (fterm_of_expr tf)
    | None -> failwith "if-then-else must be balanced")
  | Pexp_fun (_, _, arg, body) ->
    let body_t = fterm_of_expr body in
    mk_fun cur_loc [ fterm_of_pattern arg ] body_t
  | Pexp_tuple el -> mk_tup cur_loc (List.map ~f:fterm_of_expr el)
  | Pexp_construct (constr, args) ->
    (match simple_ident_of_longident constr.txt with
    | Some "true" -> mk_const cur_loc T.Constant.CTrue
    | Some "false" -> mk_const cur_loc T.Constant.CFalse
    | Some id ->
      (match args with
      | Some { pexp_desc = Pexp_tuple tl; _ } ->
        mk_data cur_loc id (List.map ~f:fterm_of_expr tl)
      | Some { pexp_desc = Pexp_ident argid; pexp_loc = argloc; _ } ->
        (match simple_ident_of_longident argid.txt with
        | Some argid -> mk_data cur_loc id [ mk_var (wloc argloc) argid ]
        | None -> failwith "Bad constructor arguments")
      | Some e -> mk_data cur_loc id [ fterm_of_expr e ]
      | None -> mk_data cur_loc id [])
    | None -> failwith "Longident for constructor not supported.")
  | Pexp_apply (fun_or_op, args) ->
    let default () =
      let funct = fterm_of_expr fun_or_op in
      let fargs = List.map ~f:(fun (_, x) -> fterm_of_expr x) args in
      mk_app cur_loc funct fargs
    in
    (match fun_or_op.pexp_desc with
    | Pexp_ident idnt ->
      (match op_fterm_of_args cur_loc idnt.txt args with
      | Some ft -> ft
      | None -> default ())
    | _ -> default ())
  | Pexp_let (Asttypes.Nonrecursive, bindings, body) ->
    (match bindings with
    | [ vb ] ->
      let bound = fterm_of_pattern vb.pvb_pat in
      let bin = fterm_of_expr vb.pvb_expr in
      mk_let cur_loc bound bin (fterm_of_expr body)
    | _ -> failwith "Only one binding in let supported.")
  (* Extension : [%synt name-of-function] introduce a function to be synthesized. *)
  | Pexp_extension (name, payload) ->
    if String.equal name.txt synt_extension
    then (
      match payload with
      | PStr
          [ { pstr_desc =
                Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident ident; _ }; _ }, _)
            ; _
            }
          ] ->
        add_synt_payload ident;
        mk_var cur_loc ident
      | _ -> failwith Fmt.(str "Unsupported payload in  %s." name.txt))
    else failwith Fmt.(str "Unsupported extension name %s." name.txt)
  | _ -> failwith (Fmt.str "Expression not supported: %a." (Printast.expression 0) expr)

and fterm_of_pattern (pat : pattern) =
  match pat.ppat_desc with
  | Ppat_var v -> mk_var (wloc pat.ppat_loc) v.txt
  | Ppat_tuple t -> mk_tup (wloc pat.ppat_loc) (List.map ~f:fterm_of_pattern t)
  | Ppat_any -> mk_any (wloc pat.ppat_loc)
  | _ ->
    failwith
      (Fmt.str "Pattern %a not supported in fterm_of_pattern." Pprintast.pattern pat)

and op_fterm_of_args
    loc
    (name : Longident.t)
    (args : (Asttypes.arg_label * expression) list)
  =
  let of_single_ident typ_param ident =
    match T.Binop.of_string ~typ_param ident, args with
    | Some bop, [ (Asttypes.Nolabel, arg1); (Asttypes.Nolabel, arg2) ] ->
      Some (mk_bin loc bop (fterm_of_expr arg1) (fterm_of_expr arg2))
    | _ ->
      (match T.Unop.of_string ~typ_param ident, args with
      | Some uop, [ (Asttypes.Nolabel, arg) ] -> Some (mk_un loc uop (fterm_of_expr arg))
      | Some uop, _ -> failwith Fmt.(str "%a has more than one argument." T.Unop.pp uop)
      | _ -> None)
  in
  match Longident.flatten name with
  | [ single_ident ] -> of_single_ident None single_ident
  | [ maybe_module_name; module_function ] ->
    (match get_module maybe_module_name with
    | Some (TSet typ_param) -> of_single_ident (Some typ_param) module_function
    | _ -> None)
  | _ -> None
;;
