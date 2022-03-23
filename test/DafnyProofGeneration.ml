open Codegen.Dafny
open Lang.Term
open Algo.Env

let test () =
  let ctx = env_create () in
  let list_datatype_decl : d_toplevel =
    let list_type = mk_named_type "list" in
    let constructors : d_datatype_constr_decl list =
      [ mk_datatype_constr "Nil" []
      ; mk_datatype_constr "Cons" [ Some "hd", mk_int_type; Some "tl", list_type ]
      ]
    in
    mk_toplevel (mk_datatype_decl "list" constructors)
  in
  let example_lemma : d_toplevel =
    let signature = mk_method_sig [ "x", mk_int_type; "y", mk_int_type ] in
    let spec =
      mk_simple_spec
        ~ensures:
          [ mk_bin
              Binop.Lt
              (mk_var ctx.ctx (Variable.mk ctx.ctx "x"))
              (mk_var ctx.ctx (Variable.mk ctx.ctx "x"))
          ]
          (* x > y *)
        ~requires:[]
        DSpecMethod
    in
    let body = Body "x + y;" in
    mk_toplevel (mk_lemma "example" signature spec body)
  in
  let example_lemma2 : d_toplevel =
    let sign = mk_method_sig [ "a", mk_int_type ] in
    let spec = mk_simple_spec ~ensures:[] ~requires:[] DSpecMethod in
    let body = Body "a" in
    mk_toplevel (mk_lemma "example2" sign spec body)
  in
  let example_program : d_program =
    { dp_includes = [ "list" ]
    ; dp_topdecls = [ list_datatype_decl; example_lemma; example_lemma2 ]
    }
  in
  Fmt.(
    pf
      stdout
      "Dafny program starts next line:@.%a@."
      (ctx >- pp_d_program)
      example_program)
;;
