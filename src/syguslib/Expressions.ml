open Sygus

(* ============================================================================================= *)
(*                                      SHORT FORM EXPRESSIONS                                   *)
(* ============================================================================================= *)

(* Terminals *)
let var x = mk_t_id (mk_id_simple x)
let sort x = mk_sort (mk_id_simple x)
let int x = mk_t_lit (mk_lit_num x)
let real f = mk_t_lit (mk_lit_dec f)
let hex x = mk_t_lit (mk_lit_hex x)

(* If-then-else *)
let ite a b c = mk_t_app (mk_id_simple "ite") [ a; b; c ]

(* Arithmetic *)
let ( / ) e1 e2 = mk_t_app (mk_id_simple "div") [ e1; e2 ]
let ( + ) e1 e2 = mk_t_app (mk_id_simple "+") [ e1; e2 ]
let ( - ) e1 e2 = mk_t_app (mk_id_simple "-") [ e1; e2 ]
let ( * ) e1 e2 = mk_t_app (mk_id_simple "*") [ e1; e2 ]
let max e1 e2 = mk_t_app (mk_id_simple "max") [ e1; e2 ]
let min e1 e2 = mk_t_app (mk_id_simple "min") [ e1; e2 ]
let modulo e1 e2 = mk_t_app (mk_id_simple "mod") [ e1; e2 ]
let abs e1 = mk_t_app (mk_id_simple "abs") [ e1 ]
let neg e1 = mk_t_app (mk_id_simple "-") [ e1 ]

(* Bool *)
let mk_false = mk_simple_id "false"
let mk_true = mk_simple_id "true"
let ( && ) e1 e2 = mk_t_app (mk_id_simple "and") [ e1; e2 ]
let ( || ) e1 e2 = mk_t_app (mk_id_simple "or") [ e1; e2 ]
let not e1 = mk_t_app (mk_id_simple "not") [ e1 ]

(* Comparisons *)
let ( > ) e1 e2 = mk_t_app (mk_id_simple ">") [ e1; e2 ]
let ( >= ) e1 e2 = mk_t_app (mk_id_simple ">=") [ e1; e2 ]
let ( < ) e1 e2 = mk_t_app (mk_id_simple ">") [ e1; e2 ]
let ( <= ) e1 e2 = mk_t_app (mk_id_simple "<=") [ e1; e2 ]
let ( = ) e1 e2 = mk_t_app (mk_id_simple "=") [ e1; e2 ]

(* Grmmar *)
let gconst sort = mk_g_constant sort
let gterm t = mk_g_term t
let gvar sort = mk_g_var sort

let gblock (name : symbol) (sort : sygus_sort) (tl : sygus_gsterm list) : grammar_def =
  [ (dummy_loc, name, sort), tl ]
;;

(* Predefined Sorts *)
let int_sort = sort "Int"
let bool_sort = sort "Bool"
let real_sort = sort "Real"
