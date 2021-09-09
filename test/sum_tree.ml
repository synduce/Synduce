open Codegen.Dafny
open Lang.Term

let tree_datatype_decl : d_toplevel =
  let tree_type = mk_named_type "tree" in
  let constructors : d_datatype_constr_decl list =
    [
      mk_datatype_constr "nil" [];
      mk_datatype_constr "node" [ (None, mk_int_type); (None, tree_type); (None, tree_type) ];
    ]
  in
  mk_toplevel (mk_datatype_decl "tree" constructors)

let rep_func : d_toplevel =
  let tree_type = mk_named_type "tree" in
  let signature = mk_func_sig ~returns:(None, [ tree_type ]) [ ("t", tree_type) ] in
  let spec = mk_simple_spec ~ensures:[] ~requires:[] DSpecFunction in
  let body = Body "t" in
  mk_toplevel (mk_func "rep" signature spec body)

let f_func : d_toplevel =
  let tree_type = mk_named_type "tree" in
  let signature =
    mk_func_sig ~returns:(None, [ mk_int_type ]) [ ("s", mk_int_type); ("t", tree_type) ]
  in
  let spec =
    mk_simple_spec ~decreases:[ mk_var (Variable.mk "t") ] ~ensures:[] ~requires:[] DSpecFunction
  in
  let body =
    Body
      "match t\n\
      \  case nil => s\n\
      \  case node(a, l, r) =>\n\
      \      var sum := f(s, l);\n\
      \      f(sum+a, r)"
  in
  mk_toplevel (mk_func "f" signature spec body)

let target_func : d_toplevel =
  let tree_type = mk_named_type "tree" in
  let signature = mk_func_sig ~returns:(None, [ mk_int_type ]) [ ("t", tree_type) ] in
  let spec = mk_simple_spec ~ensures:[] ~requires:[] DSpecFunction in
  let body = Body "match t\n  case nil => 0\n  case node(a, l, r)=> a + target(l) + target(r)" in
  mk_toplevel (mk_func "target" signature spec body)

let spec_func : d_toplevel =
  let tree_type = mk_named_type "tree" in
  let signature = mk_func_sig ~returns:(None, [ mk_int_type ]) [ ("t", tree_type) ] in
  let spec =
    mk_simple_spec ~decreases:[ mk_var (Variable.mk "t") ] ~ensures:[] ~requires:[] DSpecFunction
  in
  let body = Body "f(0, t)" in
  mk_toplevel (mk_func "spec" signature spec body)

let sum_lemma : d_toplevel =
  let tree_type = mk_named_type "tree" in
  let signature = mk_method_sig [ ("s", mk_int_type); ("t", tree_type) ] in
  let spec =
    mk_simple_spec
      ~decreases:[ mk_var (Variable.mk "t") ]
      ~ensures:
        [
          mk_bin Binop.Eq
            (mk_app
               (mk_var (Variable.mk "f"))
               [ mk_var (Variable.mk "s"); mk_var (Variable.mk "t") ])
            (mk_bin Binop.Plus
               (mk_var (Variable.mk "s"))
               (mk_app (mk_var (Variable.mk "target")) [ mk_var (Variable.mk "t") ]));
        ]
        (* f(s, t) == s + target(t) *)
      ~requires:[] DSpecMethod
  in
  let body =
    Body
      "match t\n\
      \  case nil => {}\n\
      \  case node(a, l, r) =>\n\
      \  {\n\
      \      sumLemma(s, l);\n\
      \      sumLemma(s + target(l) + a, r);\n\
      \      calc == {\n\
      \          f(s, t);\n\
      \          f(f(s, l) + a, r);\n\
      \          f(s + target(l) + a, r);\n\
      \          s + target(l) + a + target(r);\n\
      \          s + target(t);\n\
      \      }\n\
      \  }"
  in
  mk_toplevel (mk_lemma "sumLemma" signature spec body)

let correctness_lemma : d_toplevel =
  let tree_type = mk_named_type "tree" in
  let signature = mk_method_sig [ ("t", tree_type) ] in
  let spec =
    mk_simple_spec
      ~ensures:
        [
          mk_bin Binop.Eq
            (mk_app (mk_var (Variable.mk "target")) [ mk_var (Variable.mk "t") ])
            (mk_app
               (mk_var (Variable.mk "spec"))
               [ mk_app (mk_var (Variable.mk "rep")) [ mk_var (Variable.mk "t") ] ]);
        ]
        (* target(t)  == spec(rep(t))*)
      ~requires:[] DSpecMethod
  in
  let body =
    Body "match t\n  case nil => {}\n  case node(a, l, r) =>\n  {\n      sumLemma(0, t);\n  }"
  in
  mk_toplevel (mk_lemma "correctness_lemma" signature spec body)

let example_program : d_program =
  {
    dp_includes = [];
    dp_topdecls =
      [ tree_datatype_decl; rep_func; f_func; spec_func; target_func; sum_lemma; correctness_lemma ];
  }
;;

Fmt.(pf stdout "%a" pp_d_program example_program)
