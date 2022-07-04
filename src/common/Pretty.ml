open Base
open Lang
open Term
open Syguslib.Sygus
open Utils
open ProblemDefs

let str_of_repair (r : repair) =
  match r with
  | Lift -> "lift"
  | AddRecursiveCalls _ -> "add recursive call"
  | NoRepair -> "no repair"
;;

(* ============================================================================================= *)
(*          Pretty printing functions.                                                           *)
(* ============================================================================================= *)

let pp_equation ~(ctx : Context.t) (f : Formatter.t) (eqn : equation) =
  match eqn.eprecond with
  | Some inv ->
    Fmt.(
      pf
        f
        "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a@;@[%a@;%a@;%a@]@]@]"
        (styled `Italic (pp_term ctx))
        eqn.eterm
        (pp_subs ctx)
        eqn.eelim
        (pp_term ctx)
        inv
        (styled (`Fg `Red) string)
        "=>"
        (pp_term ctx)
        eqn.elhs
        (styled (`Fg `Red) string)
        "="
        (pp_term ctx)
        eqn.erhs)
  | None ->
    Fmt.(
      pf
        f
        "@[<hov 2>E(%a)<%a> := @;@[<hov 2>%a %a %a@]@]"
        (styled `Italic (pp_term ctx))
        eqn.eterm
        (pp_subs ctx)
        eqn.eelim
        (pp_term ctx)
        eqn.elhs
        (styled (`Fg `Red) string)
        "="
        (pp_term ctx)
        eqn.erhs)
;;

let pp_witness ~(ctx : Context.t) (f : Formatter.t) (witness : witness) : unit =
  let pp_model frmt model =
    (* Print as comma-separated list of variable -> term *)
    Fmt.(list ~sep:comma (pair ~sep:Utils.rightarrow (Variable.pp ctx) (pp_term ctx)))
      frmt
      (Map.to_alist model)
  in
  Fmt.pf
    f
    "@[M = [%a]@]@;@[for %a@]@;@[with elim. %a@]"
    pp_model
    witness.witness_model
    (pp_term ctx)
    witness.witness_eqn.eterm
    (pp_subs ctx)
    witness.witness_eqn.eelim
;;

let pp_implems
    ~(ctx : Context.t)
    (frmt : Formatter.t)
    (implems : (symbol * variable list * term) list)
  =
  let pp_single_or_tup frmt l =
    match l with
    | [] -> ()
    | [ (_, v) ] -> (pp_term ctx) frmt v
    | _ :: _ ->
      Fmt.(
        pf
          frmt
          "(%a)"
          (list ~sep:(fun _f () -> pf _f ", ") (pp_term ctx))
          (List.map ~f:second l))
  in
  let pp_implem frmt (s, args, t) =
    let f v =
      match Variable.vtype_or_new ctx v with
      | TTup tl ->
        let f i typ =
          let v' = Variable.mk ctx ~t:(Some typ) (Alpha.fresh ctx.names ~s:v.vname) in
          mk_sel ctx (mk_var ctx v) i, mk_var ctx v'
        in
        let subs1 = List.mapi ~f tl in
        let _, vars = List.unzip subs1 in
        (mk_var ctx v, mk_tup ctx vars) :: subs1, subs1
      | _ -> [], [ mk_var ctx v, mk_var ctx v ]
    in
    let subs, args = List.unzip (List.map ~f args) in
    let t' = substitution (List.concat subs) t in
    Fmt.(
      pf
        frmt
        "@[<hov 2>%a %a @[%a@] %a@;%a@]"
        (styled (`Fg `Red) string)
        "let"
        (styled (`Fg `Cyan) string)
        s
        (list ~sep:sp pp_single_or_tup)
        args
        (styled (`Fg `Red) string)
        "="
        (pp_term ctx)
        t')
  in
  Fmt.((list ~sep:(fun _f () -> pf _f "@.@.") pp_implem) frmt implems)
;;

let pp_soln
    ?(use_ocaml_syntax = false)
    ~(ctx : Context.t)
    (frmt : Formatter.t)
    (solution : soln)
  =
  Fmt.(
    pf
      frmt
      "@.%a@.@.@[%a@]@."
      (pp_implems ~ctx)
      solution.soln_implems
      (if use_ocaml_syntax
      then PMRS.pp_ocaml ~ctx ~short:false
      else PMRS.pp ~ctx ~short:false)
      solution.soln_rec_scheme)
;;
