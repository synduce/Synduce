open Base
open Sygus
open Parser
open Serializer

(* ============================================================================================= *)
(*                                      WRAPPER MODULES                                          *)
(* ============================================================================================= *)

module Command = struct
  type t = command

  let of_sexp x = command_of_asexp (Annot.of_sexp x)
  let sexp_of = sexp_of_command
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module Term = struct
  type t = sygus_term

  let of_sexp x = sygus_term_of_asexp (Annot.of_sexp x)
  let sexp_of = sexp_of_sygus_term
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module Ident = struct
  type t = identifier

  let of_sexp x = identifier_of_asexp (Annot.of_sexp x)
  let sexp_of = sexp_of_identifier
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module Lit = struct
  type t = literal

  let of_sexp x = literal_of_asexp (Annot.of_sexp x)
  let sexp_of = sexp_of_literal
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module Sort = struct
  type t = sygus_sort

  let of_sexp x = sygus_sort_of_asexp (Annot.of_sexp x)
  let sexp_of = sexp_of_sygus_sort
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

(* ============================================================================================= *)
(*                       SEMANTIC PROPERTIES                                                     *)
(* ============================================================================================= *)

let is_setter_command (c : Command.t) =
  match c with
  | CSetFeature _ | CSetInfo _ | CSetLogic _ | CSetOption _ -> true
  | _ -> false
;;

let is_well_formed (p : program) : bool =
  let setter_then_other l =
    let _, b =
      List.fold l ~init:(false, true) ~f:(fun (b0, b1) c ->
          let b = is_setter_command c in
          b0 && b, b1 && ((not b) || (b && b0)))
    in
    b
  in
  match p with
  | [] -> true
  | hd :: tl ->
    (match hd with
    | CSetLogic _ -> setter_then_other tl
    | _ -> setter_then_other p)
;;

let declares (c : command) : symbol list =
  match c with
  | CCheckSynth _
  | CInvConstraint _
  | CSetFeature _
  | CSetInfo _
  | CSetOption _
  | CSetLogic _
  | CAssume _
  | COptimizeSynth _
  | CChcConstraint _
  | CConstraint _ -> []
  | COracle (_, ODeclareFun (s, _, _, _)) -> [ s ]
  | COracle _ -> []
  | CDeclareVar (_, s, _)
  | CSynthFun (_, s, _, _, _)
  | CSynthInv (_, s, _, _)
  | CDeclareSort (_, s, _)
  | CDefineFun (_, s, _, _, _)
  | CDefineSort (_, s, _) -> [ s ]
  | CDeclareWeight (_, s, _) -> [ s ]
  | CDeclareDataType (_, s, constrs) -> s :: List.map ~f:fst constrs
  | CDeclareDataTypes (_, sl, cd) ->
    List.map ~f:fst sl @ List.concat_map ~f:(List.map ~f:fst) cd
;;

let compare_declares (c1 : command) (c2 : command) =
  List.compare String.compare (declares c1) (declares c2)
;;

(* ============================================================================================= *)
(*                       STATIC DEFINITIONS                                                      *)
(* ============================================================================================= *)

let max_definition =
  Command.of_sexp
    (Parsexp.Single.parse_string_exn
       "(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))")
;;

let min_definition =
  Command.of_sexp
    (Parsexp.Single.parse_string_exn
       "(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))")
;;

(* ============================================================================================= *)
(*                       TRANSFORMERS                                                            *)
(* ============================================================================================= *)
let rec rename (subs : (symbol * symbol) list) (t : sygus_term) : sygus_term =
  match t with
  | SyId (loc1, IdSimple (loc2, s)) ->
    (match List.Assoc.find ~equal:String.equal subs s with
    | Some s' -> mk_t_id ~loc:loc1 (mk_id_simple ~loc:loc2 s')
    | None -> t)
  | SyApp (loc1, IdSimple (loc2, f), args) ->
    let args' = List.map ~f:(rename subs) args in
    (match List.Assoc.find ~equal:String.equal subs f with
    | Some f' -> SyApp (loc1, IdSimple (loc2, f'), args')
    | None -> SyApp (loc1, IdSimple (loc2, f), args'))
  | SyApp (loc, f, args) -> SyApp (loc, f, List.map ~f:(rename subs) args)
  | SyExists (loc, vars, body) ->
    let subs' =
      List.filter
        ~f:(fun (l, _) ->
          List.Assoc.mem ~equal:String.equal (List.map ~f:(fun (_, a, b) -> a, b) vars) l)
        subs
    in
    SyExists (loc, vars, rename subs' body)
  | SyForall (loc, vars, body) ->
    let subs' =
      List.filter
        ~f:(fun (l, _) ->
          List.Assoc.mem ~equal:String.equal (List.map ~f:(fun (_, a, b) -> a, b) vars) l)
        subs
    in
    SyForall (loc, vars, rename subs' body)
  | SyLit _ | SyId _ -> t
  | SyLet (loc, bindings, body) ->
    let bindings' =
      List.map ~f:(fun (loc, varname, body) -> loc, varname, rename subs body) bindings
    in
    let subs' =
      List.filter
        ~f:(fun (l, _) ->
          List.Assoc.mem
            ~equal:String.equal
            (List.map ~f:(fun (_, a, b) -> a, b) bindings)
            l)
        subs
    in
    let body' = rename subs' body in
    SyLet (loc, bindings', body')
;;

let write_command (out : Stdio.Out_channel.t) (c : command) : unit =
  let comm_s = Fmt.(to_to_string Command.pp c) in
  Stdio.Out_channel.(
    output_lines out [ comm_s ];
    flush out)
;;
