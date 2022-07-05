(**
   This implementation is based on the SyGuS Language Standard Version 2.0.
   Documentation can be found at https://sygus.org/language/
*)
open Base

let use_v1 = ref false

type location = Parsexp.Positions.range
type position = Parsexp.Positions.pos

let dummy_loc : location =
  { start_pos = { line = -1; col = -1; offset = 0 }
  ; end_pos = { line = -1; col = -1; offset = 0 }
  }
;;

let pp_pos (frmt : Formatter.t) (pos : position) = Fmt.pf frmt "%i:%i" pos.line pos.col

let pp_loc (frmt : Formatter.t) (loc : location) =
  Fmt.pf frmt "%a-%a" pp_pos loc.start_pos pp_pos loc.end_pos
;;

(* ============================================================================================= *)
(*                               TYPES FOR SYGUS SYNTAX                                          *)
(* ============================================================================================= *)

type symbol = string

type attribute =
  | Attr of location * symbol
  | AttrVal of location * symbol * string

type literal =
  | LitNum of location * int
  | LitDec of location * float
  | LitBool of location * bool
  | LitHex of location * string
  | LitBin of location * bool list
  | LitString of location * string

type index =
  | INum of location * int
  | ISym of location * symbol

type identifier =
  | IdSimple of location * symbol
  | IdIndexed of location * symbol * index list
  | IdQual of location * symbol * sygus_sort

and sygus_sort =
  | SId of location * identifier
  | SApp of location * identifier * sygus_sort list

type sygus_term =
  | SyId of location * identifier
  | SyLit of location * literal
  | SyApp of location * identifier * sygus_term list
  | SyExists of location * sorted_var list * sygus_term
  | SyForall of location * sorted_var list * sygus_term
  | SyLet of location * binding list * sygus_term

and sorted_var = location * symbol * sygus_sort
and binding = location * symbol * sygus_term

type feature =
  | FGrammar
  | FFwdDecls
  | FRecursion
  | FOracles
  | FWeights

type command =
  | CCheckSynth of location
  | CAssume of location * sygus_term
  | CConstraint of location * sygus_term
  | CChcConstraint of location * sorted_var list * sygus_term * sygus_term
  | CDeclareVar of location * symbol * sygus_sort
  | CDeclareWeight of location * symbol * attribute list
  | CInvConstraint of location * symbol * symbol * symbol * symbol
  | CSetFeature of location * feature * bool
  | CSynthFun of location * symbol * sorted_var list * sygus_sort * grammar_def option
  | CSynthInv of location * symbol * sorted_var list * grammar_def option
  | COptimizeSynth of location * sygus_term list * attribute list
  | CDeclareDataType of location * symbol * dt_cons_dec list
  | CDeclareDataTypes of location * sygus_sort_decl list * dt_cons_dec list list
  | CDeclareSort of location * symbol * int
  | CDefineFun of location * symbol * sorted_var list * sygus_sort * sygus_term
  | CDefineSort of location * symbol * sygus_sort
  | CSetInfo of location * symbol * literal
  | CSetLogic of location * symbol
  | CSetOption of location * symbol * literal
  | COracle of location * oracle_command

and oracle_command =
  | OAssume of sorted_var list * sorted_var list * sygus_term * symbol
  | OConstraint of sorted_var list * sorted_var list * sygus_term * symbol
  | ODeclareFun of symbol * sygus_sort list * sygus_sort * symbol
  | OConstraintIO of symbol * symbol
  | OConstraintCex of symbol * symbol
  | OConstraintMem of symbol * symbol
  | OConstraintPosw of symbol * symbol
  | OConstraintNegw of symbol * symbol
  | OCorrectness of symbol * symbol
  | OCorrectnessCex of symbol * symbol

and sygus_sort_decl = symbol * int
and dt_cons_dec = symbol * sorted_var list
and grammar_def = (sorted_var * grouped_rule_list) list
and grouped_rule_list = sygus_gsterm list

and sygus_gsterm =
  | GConstant of location * sygus_sort
  | GTerm of location * sygus_term
  | GVar of location * sygus_sort

type program = command list

let special_chars : char list =
  [ '_'; '+'; '-'; '*'; '&'; '|'; '!'; '~'; '<'; '>'; '='; '/'; '%'; '?'; '.'; '$'; '^' ]
;;

let reserved_words : string list =
  [ "check-synth"
  ; "Constant"
  ; "constraint"
  ; "declare-datatype"
  ; "declare-datatypes"
  ; "declare-sort"
  ; "declare-var"
  ; "define-fun"
  ; "define-sort"
  ; "exists"
  ; "forall"
  ; "inv-constraint"
  ; "let"
  ; "set-feature"
  ; "set-info"
  ; "set-logic"
  ; "set-option"
  ; "synth-fun"
  ; "synth-inv"
  ; "Variable"
  ]
;;

let digits = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

(**
   `valid_ident name` is `true` whenever `name` is not a reserved word, and does not contain
   any special character.
*)
let valid_ident (name : string) =
  (not (List.mem ~equal:String.equal reserved_words name))
  &&
  match String.to_list name with
  | hd :: _ -> not (List.mem ~equal:Char.equal digits hd)
  | [] -> false
;;

let char_to_bool (c : char) = if Char.(c = '0') then false else true

let has_standard_extension (s : string) =
  try
    let ext = Caml.Filename.extension s in
    String.equal ext ".sl"
  with
  | _ -> false
;;

(* ============================================================================================= *)
(*                                      CONSTRUCTING TERMS                                       *)
(* ============================================================================================= *)

let mk_symbol s = s

(* Attributes *)

let mk_attr ?(loc = dummy_loc) (s : symbol) = Attr (loc, s)
let mk_attr_val ?(loc = dummy_loc) (s : symbol) (v : string) = AttrVal (loc, s, v)

(* Literals *)
let mk_lit_num ?(loc = dummy_loc) (i : int) = LitNum (loc, i)
let mk_lit_dec ?(loc = dummy_loc) (d : float) = LitDec (loc, d)
let mk_lit_bool ?(loc = dummy_loc) (b : bool) = LitBool (loc, b)
let mk_lit_hex ?(loc = dummy_loc) (s : string) = LitHex (loc, s)
let mk_lit_bin ?(loc = dummy_loc) (bits : bool list) = LitBin (loc, bits)
let mk_lit_string ?(loc = dummy_loc) (s : string) = LitString (loc, s)

(* Shortcuts for terms *)

let mk_num ?(loc = dummy_loc) (i : int) = SyLit (loc, LitNum (loc, i))
let mk_dec ?(loc = dummy_loc) (f : float) = SyLit (loc, LitDec (loc, f))
let mk_bool ?(loc = dummy_loc) (b : bool) = SyLit (loc, LitBool (loc, b))
let mk_hex ?(loc = dummy_loc) (h : string) = SyLit (loc, LitHex (loc, h))
let mk_bin ?(loc = dummy_loc) (b : bool list) = SyLit (loc, LitBin (loc, b))
let mk_string ?(loc = dummy_loc) (s : string) = SyLit (loc, LitString (loc, s))

(* Index *)

let mk_index_num ?(loc = dummy_loc) (i : int) = INum (loc, i)
let mk_index_sym ?(loc = dummy_loc) (i : string) = ISym (loc, i)

(* Identifier *)
let mk_id_simple ?(loc = dummy_loc) (i : string) = IdSimple (loc, i)

let mk_id_indexed ?(loc = dummy_loc) (i : string) (sub : index list) =
  IdIndexed (loc, i, sub)
;;

let mk_id_qual ?(loc = dummy_loc) (i : string) (qual : sygus_sort) = IdQual (loc, i, qual)

(* Sorts *)
let mk_sort ?(loc = dummy_loc) (i : identifier) = SId (loc, i)

let mk_sort_app ?(loc = dummy_loc) (i : identifier) (args : sygus_sort list) =
  SApp (loc, i, args)
;;

(* Terms *)
let mk_t_id ?(loc = dummy_loc) (i : identifier) = SyId (loc, i)
let mk_simple_id ?(loc = dummy_loc) (s : symbol) = SyId (loc, IdSimple (loc, s))
let mk_t_lit ?(loc = dummy_loc) (l : literal) = SyLit (loc, l)

let mk_t_app ?(loc = dummy_loc) (f : identifier) (args : sygus_term list) =
  SyApp (loc, f, args)
;;

let mk_t_exists ?(loc = dummy_loc) (q : sorted_var list) (t : sygus_term) =
  SyExists (loc, q, t)
;;

let mk_t_forall ?(loc = dummy_loc) (q : sorted_var list) (t : sygus_term) =
  SyForall (loc, q, t)
;;

let mk_t_let ?(loc = dummy_loc) (b : binding list) (t : sygus_term) = SyLet (loc, b, t)

let mk_binding ?(loc = dummy_loc) (name : string) (t : sygus_term) : binding =
  loc, name, t
;;

let mk_sorted_var ?(loc = dummy_loc) (name : string) (sort : sygus_sort) : sorted_var =
  loc, name, sort
;;

let mk_c_check_synth ?(loc = dummy_loc) () = CCheckSynth loc
let mk_c_assume ?(loc = dummy_loc) (t : sygus_term) = CAssume (loc, t)
let mk_c_constraint ?(loc = dummy_loc) (t : sygus_term) = CConstraint (loc, t)

let mk_c_chc_constraint
    ?(loc = dummy_loc)
    (args : sorted_var list)
    (pre : sygus_term)
    (post : sygus_term)
  =
  CChcConstraint (loc, args, pre, post)
;;

let mk_c_declare_var ?(loc = dummy_loc) (name : symbol) (sort : sygus_sort) =
  CDeclareVar (loc, name, sort)
;;

let mk_c_declare_weight ?(loc = dummy_loc) (name : symbol) (attrs : attribute list) =
  CDeclareWeight (loc, name, attrs)
;;

let mk_c_inv_constraint
    ?(loc = dummy_loc)
    (s : symbol)
    (spre : symbol)
    (strans : symbol)
    (spost : symbol)
  =
  CInvConstraint (loc, s, spre, strans, spost)
;;

let mk_c_set_feature ?(loc = dummy_loc) (f : feature) (flag : bool) =
  CSetFeature (loc, f, flag)
;;

let mk_c_synth_fun
    ?(loc = dummy_loc)
    ?(g = None)
    (name : symbol)
    (fargs : sorted_var list)
    (res : sygus_sort)
  =
  CSynthFun (loc, name, fargs, res, g)
;;

let mk_c_synth_inv
    ?(loc = dummy_loc)
    ?(g = None)
    (name : symbol)
    (fargs : sorted_var list)
  =
  mk_c_synth_fun ~loc ~g name fargs (mk_sort ~loc (mk_id_simple ~loc "Bool"))
;;

let mk_c_optimize_synth
    ?(loc = dummy_loc)
    (args : sygus_term list)
    (attrs : attribute list)
  =
  COptimizeSynth (loc, args, attrs)
;;

let mk_c_declare_datatype ?(loc = dummy_loc) (name : string) (constrs : dt_cons_dec list) =
  CDeclareDataType (loc, name, constrs)
;;

let mk_c_declare_datatypes
    ?(loc = dummy_loc)
    (types : sygus_sort_decl list)
    (constrs : dt_cons_dec list list)
  =
  CDeclareDataTypes (loc, types, constrs)
;;

let mk_c_declare_sort ?(loc = dummy_loc) (name : symbol) (index : int) =
  CDeclareSort (loc, name, index)
;;

let mk_c_define_fun
    ?(loc = dummy_loc)
    (name : symbol)
    (args : sorted_var list)
    (ret : sygus_sort)
    (body : sygus_term)
  =
  CDefineFun (loc, name, args, ret, body)
;;

let mk_c_define_sort ?(loc = dummy_loc) (name : symbol) (sort : sygus_sort) =
  CDefineSort (loc, name, sort)
;;

let mk_c_set_info ?(loc = dummy_loc) (name : symbol) (value : literal) =
  CSetInfo (loc, name, value)
;;

let mk_c_set_logic ?(loc = dummy_loc) (logic : string) = CSetLogic (loc, logic)

let mk_c_set_option ?(loc = dummy_loc) (opt : string) (value : literal) =
  CSetOption (loc, opt, value)
;;

let mk_c_oracle ?(loc = dummy_loc) (oc : oracle_command) = COracle (loc, oc)

(* Grammar *)
let mk_g_constant ?(loc = dummy_loc) (sort : sygus_sort) = GConstant (loc, sort)
let mk_g_term ?(loc = dummy_loc) (term : sygus_term) = GTerm (loc, term)
let mk_g_var ?(loc = dummy_loc) (sort : sygus_sort) = GVar (loc, sort)

(* ============================================================================================= *)
(*                                      SOLVER RESPONSES                                         *)
(* ============================================================================================= *)

type solver_response =
  | RSuccess of (symbol * sorted_var list * sygus_sort * sygus_term) list
  | RInfeasible
  | RFail
  | RUnknown

let is_sat = function
  | RSuccess _ -> true
  | _ -> false
;;

let is_failed = function
  | RFail -> true
  | _ -> false
;;

let is_infeasible = function
  | RInfeasible -> true
  | _ -> false
;;
