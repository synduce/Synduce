open Base
open Option.Let_syntax
open Syguslib
open Sygus
open Term
open Utils
module E = Syguslib.Expressions

(* Instantiate solver functor in interface. *)

module SygusSolver =
  Syguslib.Solvers.LwtSolver (* Statistics collection config. *)
    (Stats)
    (* Logging config. *)
    (struct
      let error = Log.error
      let debug = Log.debug
      let verb = Log.verbose
      let verbose = false
      let log_queries = true
      let log_file = Caml.Filename.temp_file "synt" ".sl"
    end)
    (* Solvers config. *)
    (struct
      let cvc_binary_path = Config.cvc_binary_path
      let dryadsynth_binary_path () = Config.dryadsynth_binary_path
      let eusolver_binary_path () = Config.eusolver_binary_path
      let using_cvc5 = Config.using_cvc5
    end)

let rec rtype_of_sort ~(ctx : Context.t) (s : sygus_sort) : RType.t option =
  match s with
  | SId (_, IdSimple (_, sname)) ->
    (match RType.get_type ctx.types sname with
    | Some x ->
      (match x with
      | RType.TParam (_, maint) -> Some maint
      | _ -> Some x)
    | None ->
      (match Tuples.types_of_tuple_name sname with
      | Some tl -> Some (RType.TTup tl)
      | None -> None))
  | SApp (_, IdSimple (_, "Tuple"), sorts) ->
    (match all_or_none (List.map ~f:(rtype_of_sort ~ctx) sorts) with
    | Some l -> Some (RType.TTup l)
    | _ -> None)
  | SApp (_, IdSimple (_, "Set"), [ sort ]) ->
    let%bind t = rtype_of_sort ~ctx sort in
    Some (RType.TSet t)
  | SApp (_, IdSimple (_, sname), sort_params) ->
    let%bind x = RType.get_type ctx.types sname in
    let%bind y = all_or_none (List.map ~f:(rtype_of_sort ~ctx) sort_params) in
    (match x with
    | RType.TParam (params, maint) ->
      (match List.zip params y with
      | Ok l -> Some (RType.TParam (y, RType.sub_all l maint))
      | _ -> None)
    | _ -> None)
  | SId _ ->
    Log.error_msg "Indexed / qualified sorts not implemented.";
    None
  | SApp (_, _, _) ->
    Log.error_msg "Indexed sorts not implemented.";
    None
;;

let rec sort_of_rtype ~(ctx : Context.t) (t : RType.t) : sygus_sort =
  match t with
  | RType.TInt -> E.int_sort
  | RType.TBool -> E.bool_sort
  | RType.TString -> E.sort "String"
  | RType.TChar -> E.sort "Char"
  | RType.TSet s -> mk_sort_app (mk_id_simple "Set") [ sort_of_rtype ~ctx s ]
  | RType.TNamed s -> E.sort s
  | RType.TTup tl ->
    if !Config.using_cvc4_tuples
    then mk_sort_app (mk_id_simple "Tuple") (List.map ~f:(sort_of_rtype ~ctx) tl)
    else E.sort (Tuples.type_name_of_types tl)
  | RType.TFun (tin, tout) ->
    (* Functions should be unpacked before! *)
    mk_sort_app (mk_id_simple "->") [ sort_of_rtype ~ctx tin; sort_of_rtype ~ctx tout ]
  | RType.TParam (args, t) -> dec_parametric ~ctx t args
  | RType.TVar _ -> E.int_sort
(* TODO: declare sort? *)

and dec_parametric ~ctx t args =
  match t with
  | RType.TParam _ -> failwith "only one level of parameters supported in types."
  | RType.TNamed s -> mk_sort_app (mk_id_simple s) (List.map ~f:(sort_of_rtype ~ctx) args)
  | t -> sort_of_rtype ~ctx t
;;

(* Not really parametric? *)

(* ======= LOGIC HELPERS ======= *)

let requires_dt_theory ~(ctx : Context.t) (t : RType.t) =
  let rec aux t =
    let open RType in
    match t with
    | TTup _ -> true
    | TSet _ | TInt | TBool | TChar | TString | TVar _ -> false
    | TFun (a, b) -> aux a || aux b
    | TParam (tl, t) -> List.exists ~f:aux tl || aux t
    | TNamed _ -> is_datatype ctx.types t
  in
  aux t
;;

let requires_set_theory ~(ctx : Context.t) (t : RType.t) =
  let rec aux t =
    let open RType in
    match t with
    | TSet _ -> true
    | TInt | TBool | TChar | TString | TVar _ -> false
    | TFun (a, b) -> aux a || aux b
    | TParam (tl, t) -> List.exists ~f:aux tl || aux t
    | TTup tl -> List.exists ~f:aux tl
    | TNamed _ -> is_datatype ctx.types t
  in
  aux t
;;

let logic_of_operators ?(nonlinear = false) (opset : OpSet.t) : string =
  if (not nonlinear) && Set.for_all opset ~f:Operator.is_lia then "LIA" else "NIA"
;;

let dt_extend_base_logic (base_logic : string) : string = "DT" ^ base_logic
(* TODO : Z3 has other names like DT_LIA instead of DTLIA? *)

(* ======= TERM CONVERSION  ======= *)

let sygus_term_of_const ~(ctx : Context.t) (c : Constant.t) : sygus_term =
  match c with
  | Constant.CInt i -> if i >= 0 then E.int i else E.neg (E.int (-i))
  | Constant.CChar c -> mk_t_lit (mk_lit_string (String.of_char c))
  | Constant.CTrue -> E.mk_true
  | Constant.CFalse -> E.mk_false
  | Constant.CEmptySet t -> mk_t_id (mk_id_qual "emptyset" (sort_of_rtype ~ctx (TSet t)))
;;

let rec sygus_of_term ~(ctx : Context.t) (t : term) : sygus_term =
  let tk = t.tkind in
  match tk with
  | TBox t -> (sygus_of_term ~ctx) t
  | TBin (op, t1, t2) ->
    mk_t_app
      (mk_id_simple (Binop.to_string op))
      (List.map ~f:(sygus_of_term ~ctx) [ t1; t2 ])
  | TUn (op, t1) ->
    mk_t_app (mk_id_simple (Unop.to_string op)) [ (sygus_of_term ~ctx) t1 ]
  | TConst c -> sygus_term_of_const ~ctx c
  | TVar x -> E.var x.vname
  | TIte (c, a, b) ->
    E.ite ((sygus_of_term ~ctx) c) ((sygus_of_term ~ctx) a) ((sygus_of_term ~ctx) b)
  | TTup tl ->
    if !Config.using_cvc4_tuples
    then mk_t_app (mk_id_simple "mkTuple") (List.map ~f:(sygus_of_term ~ctx) tl)
    else (
      let constructor = Tuples.constr_name_of_types (List.map ~f:Term.type_of tl) in
      mk_t_app (mk_id_simple constructor) (List.map ~f:(sygus_of_term ~ctx) tl))
  | TSel (t, i) ->
    if !Config.using_cvc4_tuples
    then mk_t_app (mk_id_indexed "tupSel" [ mk_index_num i ]) [ (sygus_of_term ~ctx) t ]
    else (
      match Term.type_of t with
      | TTup tl ->
        let proj_name = Tuples.proj_name_of_types tl i in
        mk_t_app (mk_id_simple proj_name) [ (sygus_of_term ~ctx) t ]
      | _ -> failwith "Sygus: converting a tuple projection on a non-tuple term.")
  | TData (cstr, args) ->
    (match args with
    | [] -> E.var cstr
    | _ -> mk_t_app (mk_id_simple cstr) (List.map ~f:(sygus_of_term ~ctx) args))
  | TApp ({ tkind = TFun (formal_args, fun_body); _ }, args) ->
    (match List.zip formal_args args with
    | Ok pre_bindings ->
      let bindings, subs = make_bindings ~ctx pre_bindings in
      let fbody' = sygus_of_term ~ctx (substitution subs fun_body) in
      mk_t_let bindings fbody'
    | Unequal_lengths ->
      failwith "Sygus: cannot translate application with wrong number of arguments.")
  | TApp (t, []) -> (sygus_of_term ~ctx) t
  | TApp ({ tkind = TVar v; _ }, args) ->
    mk_t_app (mk_id_simple v.vname) (List.map ~f:(sygus_of_term ~ctx) args)
  | TApp (_, _) ->
    failwith
      Fmt.(str "Sygus: function application cannot be translated (%a)." (pp_term ctx) t)
  | TMatch (_, _) -> failwith "Sygus: match cases not supported."
  | TFun (_, _) -> failwith "Sygus: functions in terms not supported."

and make_bindings ~ctx pre_bindings =
  let bindings_of_varmap varmap =
    List.map
      ~f:(fun (v, t) -> mk_binding v.vname (sygus_of_term ~ctx t))
      (Map.to_alist varmap)
  in
  let make_one_binding bto bdg =
    let tto = fpat_to_term bto in
    (* Try to transform the function into a let-binding, by first creating a tuple. *)
    match Matching.matches ~ctx ~pattern:tto (tuplify ctx bdg) with
    | Some varmap -> bindings_of_varmap varmap, []
    | None ->
      (match Matching.matches ~ctx ~pattern:tto bdg with
      | Some varmap -> bindings_of_varmap varmap, []
      | None ->
        (match tto.tkind with
        | TTup tl ->
          (* Replace tuple parts that are bound by a single variable. *)
          let tl_typ = RType.TTup (List.map ~f:type_of tl) in
          let tup_var =
            Variable.mk ctx ~t:(Some tl_typ) (Alpha.fresh ~s:"tup" ctx.names)
          in
          ( [ mk_binding tup_var.vname ((sygus_of_term ~ctx) bdg) ]
          , List.mapi ~f:(fun i t -> t, mk_sel ctx (Term.mk_var ctx tup_var) i) tl )
        | _ ->
          failwith
            (Fmt.str
               "%a cannot match %a or %a in sygus conversion."
               (pp_term ctx)
               (fpat_to_term bto)
               (pp_term ctx)
               (tuplify ctx bdg)
               (pp_term ctx)
               bdg)))
  in
  List.fold ~init:([], []) pre_bindings ~f:(fun (binds, subs) (bto, bdg) ->
      let new_binds, new_subs = make_one_binding bto bdg in
      binds @ new_binds, subs @ new_subs)
;;

let constant_of_literal (l : literal) : Constant.t =
  match l with
  | LitNum (_, i) -> Constant.CInt i
  | LitBool (_, b) -> if b then Constant.CTrue else Constant.CFalse
  | LitDec _ -> failwith "No reals in base language."
  | LitHex _ | LitBin _ | LitString _ ->
    failwith "No hex, bin or string constants in language."
;;

type id_kind =
  | ICstr of string
  | IVar of variable
  | IBinop of Binop.t
  | IUnop of Unop.t
  | ITupleAccessor of int
  | INotDef
  | ITupleCstr
  | IIte

let id_kind_of_s ?(typ_param = None) ~ctx env s =
  let string_case s =
    match s with
    | "ite" -> IIte
    (* Old CVC4 Tuples *)
    | "mkTuple" -> ITupleCstr
    | s when String.is_prefix ~prefix:"__cvc4_tuple_" s ->
      let i = Int.of_string (Str.last_chars s 1) in
      ITupleAccessor i
    (* New tuple repr for CVC5 *)
    | _ when Tuples.is_constr_name s -> ITupleCstr
    | _ ->
      (match Tuples.proj_of_proj_name s with
      | Some (_, i) -> ITupleAccessor i
      | None -> INotDef)
  in
  match Map.find env s with
  | Some v -> IVar v
  | None ->
    (match Binop.of_string ~typ_param s with
    | Some bop -> IBinop bop
    | None ->
      (match Unop.of_string ~typ_param s with
      | Some unop -> IUnop unop
      | None ->
        (match RType.type_of_variant ctx s with
        | Some _ -> ICstr s
        | None -> string_case s)))
;;

let rec term_of_sygus
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (env : (string, variable, String.comparator_witness) Map.t)
    (st : sygus_term)
    : term
  =
  match st with
  | SyId (_, IdSimple (_, s)) ->
    (match Map.find env s with
    | Some v -> mk_var ctx v
    | None -> failwith Fmt.(str "term_of_sygus: variable %s not found." s))
  | SyId (_, IdQual (_, "emptyset", sort)) ->
    (match rtype_of_sort ~ctx sort with
    | Some RType.(TSet telt) -> mk_const (CEmptySet telt)
    | _ -> failwith Fmt.(str "emptyset type not recognized"))
  | SyLit (_, l) -> mk_const (constant_of_literal l)
  | SyApp (_, IdSimple (_, s), args) ->
    let args' = List.map ~f:(term_of_sygus ~fctx ~ctx env) args in
    let typ_param =
      match args' with
      | hd :: _ ->
        (match type_of (fst (infer_type ctx hd)) with
        | RType.TSet t -> Some t
        | t' -> Some t')
      | _ -> None
    in
    (match id_kind_of_s ~typ_param ~ctx:ctx.types env s with
    | ICstr c -> mk_data ctx c args'
    | IVar v -> mk_app (mk_var ctx v) args'
    | IBinop op ->
      (match args' with
      | [ t1; t2 ] -> mk_bin op t1 t2
      | [ t1 ] when Operator.(equal (Binary op) (Binary Minus)) -> mk_un Unop.Neg t1
      | _ ->
        (match mk_assoc op args' with
        | Some t -> t
        | None ->
          Log.error_msg Fmt.(str "%a with %i arguments?" Binop.pp op (List.length args'));
          failwith Fmt.(str "Sygus: %a with more than two arguments." Binop.pp op)))
    | IUnop op ->
      (match args' with
      | [ t1 ] -> mk_un op t1
      | _ -> failwith "Sygus: a unary operator with more than one argument.")
    | IIte ->
      (match args' with
      | [ t1; t2; t3 ] -> mk_ite t1 t2 t3
      | _ -> failwith "Sygus: if-then-else should have three arguments.")
    | ITupleAccessor i ->
      (match args' with
      | [ arg ] -> mk_sel ctx arg i
      | _ -> failwith "Sygus: a tuple acessor with wrong number of arguments")
    | ITupleCstr -> mk_tup ctx args'
    | INotDef -> failwith Fmt.(str "Sygus: Undefined variable %s" s))
  | SyApp (_, IdIndexed (_, "tupSel", [ INum (_, i) ]), [ arg ]) ->
    let arg' = term_of_sygus ~fctx ~ctx env arg in
    mk_sel ctx arg' i
  | SyExists (_, _, _) -> failwith "Sygus: exists-terms not supported."
  | SyForall (_, _, _) -> failwith "Sygus: forall-terms not supported."
  (* TODO: add let-conversion. *)
  | SyLet (_, syg_bindings, syg_term) ->
    let_bindings_of_sygus ~ctx ~fctx env syg_bindings syg_term
  | _ ->
    let se = Syguslib.Serializer.sexp_of_sygus_term st in
    failwith Fmt.(str "Sygus term %a not supported." Sexp.pp_hum se)

and let_bindings_of_sygus
    ~(ctx : Context.t)
    ~(fctx : PMRS.Functions.ctx)
    (env : (string, variable, String.comparator_witness) Map.t)
    (bindings : binding list)
    (body : sygus_term)
  =
  let f (_, bsymb, bterm) =
    (* Create a fresh name and then replace every occurence of the original symbol
       by the new symbol.
    *)
    let varname = Alpha.fresh ~s:bsymb ctx.names in
    let bterm' = Semantic.rename [ bsymb, varname ] bterm in
    let var = Variable.mk ctx varname in
    ( (bsymb, var)
    , (var, term_of_sygus ~fctx ~ctx (Map.set env ~key:varname ~data:var) bterm') )
  in
  let subs, t_bindings = List.unzip (List.map ~f bindings) in
  let env' =
    List.fold ~f:(fun env (_, var) -> Map.set env ~key:var.vname ~data:var) ~init:env subs
  in
  let t_body =
    term_of_sygus
      ~fctx
      ~ctx
      env'
      (Semantic.rename (List.map ~f:(fun (s, var) -> s, var.vname) subs) body)
  in
  Reduce.reduce_term ~ctx ~fctx (mk_let ctx t_bindings t_body)
;;

let grammar_production_of_skeleton
    (skel : Skeleton.t)
    ~(ints : sygus_term)
    ~(bools : sygus_term)
    (locals : (sygus_term * sygus_sort) list)
    : sygus_term list
  =
  let rec build_prods =
    Skeleton.(
      function
      | SType t ->
        (match t with
        | TInt -> [ ints ]
        | TBool -> [ bools ]
        | TParam _ -> [ ints ]
        | _ -> [])
      | SUn (u, g) ->
        let g_prods = build_prods g in
        List.map g_prods ~f:(fun prod ->
            mk_t_app (mk_id_simple (Unop.to_string u)) [ prod ])
      | SBin (b, ta, tb) ->
        let prods_a = build_prods ta
        and prods_b = build_prods tb in
        let a_x_b = List.cartesian_product prods_a prods_b in
        List.map a_x_b ~f:(fun (proda, prodb) ->
            mk_t_app (mk_id_simple (Binop.to_string b)) [ proda; prodb ])
      | SIte (a, b, c) ->
        let prods_a = build_prods a
        and prods_b = build_prods b
        and prods_c = build_prods c in
        let a_x_b_x_c =
          List.cartesian_product prods_a (List.cartesian_product prods_b prods_c)
        in
        List.map a_x_b_x_c ~f:(fun (a, (b, c)) -> E.ite a b c)
      | SChoice c -> List.concat_map ~f:build_prods c
      | SArg arg_num ->
        (match List.nth locals arg_num with
        | Some (arg_term, _) -> [ arg_term ]
        | None -> [])
      | STuple elts ->
        let prods = Utils.cartesian_nary_product (List.map ~f:build_prods elts) in
        List.map ~f:(fun tuple_args -> mk_t_app (mk_id_simple "mkTuple") tuple_args) prods
      | STypedWith _ -> []
      | SNonGuessable -> [])
  in
  build_prods skel
;;

(* ============================================================================================= *)
(*                           COMMANDS                                                            *)
(* ============================================================================================= *)

let declare_sort_of_rtype
    ~(ctx : Context.t)
    (sname : string)
    (variants : (string * RType.t list) list)
    : command
  =
  let dt_cons_decs =
    let f (variantname, variantargs) =
      ( variantname
      , List.mapi variantargs ~f:(fun i t ->
            mk_sorted_var (variantname ^ "_" ^ Int.to_string i) (sort_of_rtype ~ctx t)) )
    in
    List.map ~f variants
  in
  mk_c_declare_datatype sname dt_cons_decs
;;

let declare_sort_of_tuple ~(ctx : Context.t) (tl : RType.t list) : string * command =
  let name = Tuples.type_name_of_types tl in
  let dt_cons_decs =
    let constr_name = Tuples.constr_name_of_types tl in
    let f i t =
      let proj_name = Tuples.proj_name_of_types tl i in
      mk_sorted_var proj_name (sort_of_rtype ~ctx t)
    in
    (* A tuple has a single constructor. *)
    [ constr_name, List.mapi ~f tl ]
  in
  name, mk_c_declare_datatype name dt_cons_decs
;;

let declare_sorts_of_var ~(ctx : Context.t) (v : variable) =
  let sort_decls = Map.empty (module String) in
  let rec f sort_decls t =
    RType.(
      match t with
      | TInt | TBool | TChar | TString -> sort_decls
      | TTup tl ->
        let sort_decls' = List.fold ~f ~init:sort_decls tl in
        let tuple_name, tuple_decl = declare_sort_of_tuple ~ctx tl in
        Map.set sort_decls' ~key:tuple_name ~data:tuple_decl
      | TNamed tname ->
        (match get_variants ctx.types t with
        | [] -> sort_decls
        | l -> Map.set sort_decls ~key:tname ~data:(declare_sort_of_rtype ~ctx tname l))
      | _ -> sort_decls)
  in
  Map.to_alist (f sort_decls (Variable.vtype_or_new ctx v))
;;

let sorted_vars_of_types ~(ctx : Context.t) (tl : RType.t list) : sorted_var list =
  let f t =
    (* Declare var for future parsing. *)
    let varname = Alpha.fresh ctx.names in
    mk_sorted_var varname (sort_of_rtype ~ctx t)
  in
  List.map ~f tl
;;

let sorted_vars_of_vars ~(ctx : Context.t) (vars : variable list) : sorted_var list =
  List.map
    ~f:(fun v -> mk_sorted_var v.vname (sort_of_rtype ~ctx (Variable.vtype_or_new ctx v)))
    vars
;;

let mk_synthfun
    ~(ctx : Context.t)
    (name : string)
    (args : variable list)
    (ret_type : RType.t)
    (grammar : grammar_def option)
    : command
  =
  mk_c_synth_fun
    name
    (sorted_vars_of_vars ~ctx args)
    (sort_of_rtype ~ctx ret_type)
    ~g:grammar
;;

let mk_synthinv
    ~(ctx : Context.t)
    (name : string)
    (args : variable list)
    (grammar : grammar_def option)
    : command
  =
  mk_c_synth_fun name (sorted_vars_of_vars ~ctx args) E.bool_sort ~g:grammar
;;

let wait_on_failure (counter : int ref) (t : (solver_response * 'a) Lwt.t)
    : (solver_response * 'a) Lwt.t
  =
  Lwt.bind t (fun t ->
      match t with
      (* Wait on failure. *)
      | (RFail | RUnknown | RInfeasible), _ ->
        let rec wait_and_check t =
          Lwt.bind (Lwt_unix.sleep 1.0) (fun _ ->
              if !counter > 1 || Float.(t < 0.)
              then wait_and_check (t -. 1.0)
              else Lwt.return ())
        in
        Lwt.map
          (fun _ -> t)
          (if !counter > 1
          then (
            Int.decr counter;
            wait_and_check !Config.Optims.wait_parallel_tlimit)
          else Lwt.return ())
      (* Continue on success. *)
      | _ ->
        Int.decr counter;
        Lwt.return t)
;;

(* ============================================================================================= *)
(*                       ABSTRACTION: SOLVER CLASS                                               *)
(* ============================================================================================= *)

let collect_tuple_decls (commands : command list) =
  let rec of_sygus_term (s : sygus_term) =
    match s with
    | SyApp (_, id, tl) ->
      (match id with
      | IdSimple (_, s) ->
        (match Tuples.type_name_of_constr s with
        | Some tname -> [ tname ]
        | None ->
          (match Tuples.type_name_of_proj_name s with
          | Some tname -> [ tname ]
          | None -> []))
      | _ -> [])
      @ List.concat_map ~f:of_sygus_term tl
    | SyForall (_, sortedvs, body) | SyExists (_, sortedvs, body) ->
      List.filter_map ~f:(fun (_, _, s) -> of_sort s) sortedvs @ of_sygus_term body
    | SyLet (_, bindings, body) ->
      List.concat_map ~f:(fun (_, _, t) -> of_sygus_term t) bindings @ of_sygus_term body
    | _ -> []
  and of_sort sort =
    match sort with
    | SId (_, IdSimple (_, s)) ->
      if Option.is_some (Tuples.types_of_tuple_name s) then Some s else None
    | _ -> None
  in
  let of_dt_cons (_, arg_sorts) =
    List.filter_map arg_sorts ~f:(fun ((_, _, sort) : 'a * 'b * sygus_sort) ->
        of_sort sort)
  in
  let of_command (c : command) =
    match c with
    | CConstraint (_, c) -> of_sygus_term c
    | CDeclareDataType (_, _, dts) -> List.concat_map ~f:of_dt_cons dts
    | CDeclareDataTypes (_, _, dtss) ->
      List.concat_map ~f:of_dt_cons (List.concat_map ~f:identity dtss)
    | CDefineSort (_, _, sort) -> Option.to_list (of_sort sort)
    | CSynthFun (_, _, args, ret_sort, _) ->
      List.filter_map args ~f:(fun (_, _, sort) -> of_sort sort)
      @ Option.to_list (of_sort ret_sort)
    | CSynthInv (_, _, args, _) ->
      List.filter_map args ~f:(fun (_, _, sort) -> of_sort sort)
    | _ -> []
  in
  Set.diff
    (Set.of_list (module String) (List.concat_map ~f:of_command commands))
    (Set.of_list (module String) (List.concat_map ~f:Semantic.declares commands))
;;

module HLSolver = struct
  type t =
    { declared : string Hash_set.t
    ; logic : string
    ; constraints : command list
    ; extra_defs : command list
    ; definitions : command list
    ; sorts : command list
    ; objs : command list
    }

  let make ?(extra_defs = []) () =
    { declared = Hash_set.create (module String)
    ; logic = "LIA"
    ; constraints = []
    ; extra_defs
    ; definitions = []
    ; sorts = []
    ; objs = []
    }
  ;;

  let declare_sort (solver : t) ((s, command) : string * command) : t =
    if Hash_set.mem solver.declared s
    then solver
    else (
      Hash_set.add solver.declared s;
      { solver with sorts = solver.sorts @ [ command ] })
  ;;

  let warning ~(ctx : Context.t) (v : variable) : unit =
    if RType.is_function (Variable.vtype_or_new ctx v)
    then
      Log.error
        Fmt.(
          fun fmt () ->
            pf
              fmt
              "Trying to declare %s in solver. Maybe you forgot do declare a synthesis \
               objective first?"
              v.vname)
  ;;

  let define_var ~(ctx : Context.t) (solver : t) (v : variable) : t =
    if Hash_set.mem solver.declared v.vname
    then solver
    else (
      warning ~ctx v;
      Hash_set.add solver.declared v.vname;
      let sorts = declare_sorts_of_var ~ctx v in
      let solver = List.fold ~f:declare_sort sorts ~init:solver in
      { solver with
        definitions =
          solver.definitions
          @ [ mk_c_declare_var v.vname (sort_of_rtype ~ctx (Variable.vtype_or_new ctx v))
            ]
      })
  ;;

  let synthesize (cl : command list) (solver : t) : t =
    let f solver c =
      match c with
      | CSynthFun (_, name, _, _, _) | CSynthInv (_, name, _, _) ->
        Hash_set.add solver.declared name;
        { solver with objs = solver.objs @ [ c ] }
      | _ -> solver
    in
    List.fold ~f ~init:solver cl
  ;;

  let constrain ~(ctx : Context.t) (terms : term list) (solver : t) : t =
    let f solver term =
      let t' = sygus_of_term ~ctx term in
      let solver =
        Set.fold ~init:solver ~f:(define_var ~ctx) (Analysis.free_variables ~ctx term)
      in
      { solver with constraints = solver.constraints @ [ mk_c_constraint t' ] }
    in
    List.fold ~init:solver ~f terms
  ;;

  let set_logic (name : string) (solver : t) : t = { solver with logic = name }

  let all_commands ~(ctx : Context.t) (solver : t) =
    let pre c = List.dedup_and_sort ~compare:Semantic.compare_declares c in
    let core =
      solver.sorts
      @ solver.extra_defs
      @ solver.objs
      @ List.rev solver.definitions
      @ solver.constraints
    in
    let tuple_decls =
      let tds = collect_tuple_decls core in
      List.filter_map
        ~f:(fun tuplename ->
          Option.map
            ~f:(fun x -> snd (declare_sort_of_tuple ~ctx x))
            (Tuples.types_of_tuple_name tuplename))
        (Set.to_list tds)
    in
    [ mk_c_set_logic solver.logic ] @ pre tuple_decls @ core @ [ mk_c_check_synth () ]
  ;;

  let solve ?(timeout = None) ~(ctx : Context.t) (solver : t) =
    let solver_kind =
      if !Config.use_eusolver
      then SygusSolver.CoreSolver.EUSolver
      else SygusSolver.CoreSolver.CVC
    in
    let commands = all_commands ~ctx solver in
    SygusSolver.solve_commands ~timeout ~solver_kind commands
  ;;

  let to_file ~(ctx : Context.t) (filename : string) (solver : t) : unit =
    Syguslib.Solvers.commands_to_file (all_commands ~ctx solver) filename
  ;;
end

let pp_response (frmt : Formatter.t) (r : solver_response) =
  let p_binding frmt (fname, args, _, body) =
    Fmt.pf
      frmt
      "%s(%a) = %a"
      fname
      (Fmt.list Sexp.pp_hum)
      (List.map ~f:Serializer.sexp_of_sorted_var args)
      Sexp.pp_hum
      (Serializer.sexp_of_sygus_term body)
  in
  match r with
  | RFail -> Fmt.pf frmt "failed"
  | RInfeasible -> Fmt.pf frmt "infeasible"
  | RUnknown -> Fmt.pf frmt "unknown"
  | RSuccess soln -> Fmt.(pf frmt "Solution %a" (list ~sep:sp p_binding) soln)
;;
