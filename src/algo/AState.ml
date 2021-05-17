open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils

(**
  AState: This module contains state variables for the synthesis algorithms, as well as type
   definitions and printing functions for displaying solutions.
 *)

type psi_def = { target : PMRS.t; orig : PMRS.t; repr : PMRS.t; repr_is_identity : bool }

let _tau = ref RType.TInt

let _theta = ref RType.TInt

let _alpha : (RType.t * Term.term option) ref = ref (RType.TInt, None)

let _span = ref 1

let refinement_steps = ref 0

type soln = { soln_rec_scheme : PMRS.t; soln_implems : (symbol * variable list * term) list }

let pp_implems (frmt : Formatter.t) (implems : (symbol * variable list * term) list) =
  let pp_single_or_tup frmt l =
    match l with
    | [] -> ()
    | [ (_, v) ] -> pp_term frmt v
    | _ :: _ ->
        Fmt.(pf frmt "(%a)" (list ~sep:(fun _f () -> pf _f ", ") pp_term) (List.map ~f:second l))
  in

  let pp_implem frmt (s, args, t) =
    let f v =
      match Variable.vtype_or_new v with
      | TTup tl ->
          let f i typ =
            let v' = Variable.mk ~t:(Some typ) (Alpha.fresh ~s:"j" ()) in
            (mk_sel (mk_var v) i, mk_var v')
          in
          List.mapi ~f tl
      | _ -> [ (mk_var v, mk_var v) ]
    in
    let subs = List.map ~f args in
    let t' = substitution (List.concat subs) t in
    Fmt.(
      pf frmt "@[<hov 2>%a %a @[%a@] %a@;%a@]"
        (styled (`Fg `Red) string)
        "let"
        (styled (`Fg `Cyan) string)
        s (list ~sep:sp pp_single_or_tup) subs
        (styled (`Fg `Red) string)
        "=" pp_term t')
  in
  Fmt.((list ~sep:(fun _f () -> pf _f "@.@.") pp_implem) frmt implems)

let pp_soln ?(use_ocaml_syntax = false) (frmt : Formatter.t) (solution : soln) =
  Fmt.(
    pf frmt "@.%a@.@.@[%a@]@." pp_implems solution.soln_implems
      (if use_ocaml_syntax then PMRS.pp_ocaml else PMRS.pp)
      solution.soln_rec_scheme)
