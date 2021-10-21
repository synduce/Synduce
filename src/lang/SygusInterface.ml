open Base
open Syguslib.Sygus
open Term
open Option.Let_syntax
open Utils

(* Instantiate solver functor in interface. *)

module SygusSolver =
  Syguslib.Solvers.SygusSolver
    (* Statistics collection config. *)
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

let rec rtype_of_sort (s : sygus_sort) : RType.t option =
  match s with
  | SId (IdSimple sname) ->
    let%bind x = RType.get_type sname in
    (match x with
    | RType.TParam (_, maint) -> Some maint
    | _ -> Some x)
  | SApp (IdSimple "Tuple", sorts) ->
    (match all_or_none (List.map ~f:rtype_of_sort sorts) with
    | Some l -> Some (RType.TTup l)
    | _ -> None)
  | SApp (IdSimple sname, sort_params) ->
    let%bind x = RType.get_type sname in
    let%bind y = all_or_none (List.map ~f:rtype_of_sort sort_params) in
    (match x with
    | RType.TParam (params, maint) ->
      (match List.zip params y with
      | Ok l -> Some (RType.TParam (y, RType.sub_all l maint))
      | _ -> None)
    | _ -> None)
  | SId _ ->
    Log.error_msg "Indexed / qualified sorts not implemented.";
    None
  | SApp (_, _) ->
    Log.error_msg "Indexed sorts not implemented.";
    None
;;

let rec sort_of_rtype (t : RType.t) : sygus_sort =
  match t with
  | RType.TInt -> SId (IdSimple "Int")
  | RType.TBool -> SId (IdSimple "Bool")
  | RType.TString -> SId (IdSimple "String")
  | RType.TChar -> SId (IdSimple "Char")
  | RType.TNamed s -> SId (IdSimple s)
  | RType.TTup tl -> SApp (IdSimple "Tuple", List.map ~f:sort_of_rtype tl)
  | RType.TFun (tin, tout) ->
    (* Functions should be unpacked before! *)
    SApp (IdSimple "->", [ sort_of_rtype tin; sort_of_rtype tout ])
  | RType.TParam (args, t) -> dec_parametric t args
  | RType.TVar _ -> SId (IdSimple "Int")
(* TODO: declare sort? *)

and dec_parametric t args =
  match t with
  | RType.TParam _ -> failwith "only one level of parameters supported in types."
  | RType.TNamed s -> SApp (IdSimple s, List.map ~f:sort_of_rtype args)
  | t -> sort_of_rtype t
;;

(* Not really parametric? *)

(* ======= LOGIC HELPERS ======= *)

let requires_dt_theory (t : RType.t) =
  let rec aux t =
    let open RType in
    match t with
    | TTup _ -> true
    | TInt | TBool | TChar | TString | TVar _ -> false
    | TFun (a, b) -> aux a || aux b
    | TParam (tl, t) -> List.exists ~f:aux tl || aux t
    | TNamed _ -> is_datatype t
  in
  aux t
;;

let logic_of_operators ?(nonlinear = false) (opset : OpSet.t) : string =
  if (not nonlinear) && Set.for_all opset ~f:Operator.is_lia then "LIA" else "NIA"
;;

let dt_extend_base_logic (base_logic : string) : string = "DT" ^ base_logic
(* TODO : Z3 has other names like DT_LIA instead of DTLIA? *)

(* ======= TERM CONVERSION  ======= *)

let sygus_term_of_const (c : Constant.t) : sygus_term =
  match c with
  | Constant.CInt i ->
    if i >= 0 then SyLit (LitNum i) else SyApp (IdSimple "-", [ SyLit (LitNum (-i)) ])
  | Constant.CTrue -> SyLit (LitBool true)
  | Constant.CFalse -> SyLit (LitBool false)
;;

let rec sygus_of_term (t : term) : sygus_term =
  let tk = t.tkind in
  match tk with
  | TBox t -> sygus_of_term t
  | TBin (op, t1, t2) ->
    SyApp (IdSimple (Binop.to_string op), List.map ~f:sygus_of_term [ t1; t2 ])
  | TUn (op, t1) -> SyApp (IdSimple (Unop.to_string op), [ sygus_of_term t1 ])
  | TConst c -> sygus_term_of_const c
  | TVar x -> SyId (IdSimple x.vname)
  | TIte (c, a, b) -> SyApp (IdSimple "ite", List.map ~f:sygus_of_term [ c; a; b ])
  | TTup tl -> SyApp (IdSimple "mkTuple", List.map ~f:sygus_of_term tl)
  | TSel (t, i) -> SyApp (IdIndexed ("tupSel", [ INum i ]), [ sygus_of_term t ])
  | TData (cstr, args) ->
    (match args with
    | [] -> SyId (IdSimple cstr)
    | _ -> SyApp (IdSimple cstr, List.map ~f:sygus_of_term args))
  | TApp ({ tkind = TVar v; _ }, args) ->
    SyApp (IdSimple v.vname, List.map ~f:sygus_of_term args)
  | TApp (_, _) ->
    failwith "Sygus: application function can only be variable. TODO: add let-conversion."
  | TMatch (_, _) -> failwith "Sygus: match cases not supported."
  | TFun (_, _) -> failwith "Sygus: functions in terms not supported."
;;

let constant_of_literal (l : literal) : Constant.t =
  match l with
  | LitNum i -> Constant.CInt i
  | LitBool b -> if b then Constant.CTrue else Constant.CFalse
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

let id_kind_of_s env s =
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
    (match Binop.of_string s with
    | Some bop -> IBinop bop
    | None ->
      (match Unop.of_string s with
      | Some unop -> IUnop unop
      | None ->
        (match RType.type_of_variant s with
        | Some _ -> ICstr s
        | None -> string_case s)))
;;

let rec term_of_sygus
    (env : (string, variable, String.comparator_witness) Map.t)
    (st : sygus_term)
    : term
  =
  match st with
  | SyId (IdSimple s) ->
    (match Map.find env s with
    | Some v -> mk_var v
    | None -> failwith Fmt.(str "term_of_sygus: variable %s not found." s))
  | SyLit l -> mk_const (constant_of_literal l)
  | SyApp (IdSimple s, args) ->
    let args' = List.map ~f:(term_of_sygus env) args in
    (match id_kind_of_s env s with
    | ICstr c -> mk_data c args'
    | IVar v -> mk_app (mk_var v) args'
    | IBinop op ->
      (match args' with
      | [ t1; t2 ] -> mk_bin op t1 t2
      | [ t1 ] when Operator.(equal (Binary op) (Binary Minus)) -> mk_un Unop.Neg t1
      | _ ->
        Log.error_msg Fmt.(str "%a with %i arguments?" Binop.pp op (List.length args'));
        failwith Fmt.(str "Sygus: %a with more than two arguments." Binop.pp op))
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
      | [ arg ] -> mk_sel arg i
      | _ -> failwith "Sygus: a tuple acessor with wrong number of arguments")
    | ITupleCstr -> mk_tup args'
    | INotDef -> failwith Fmt.(str "Sygus: Undefined variable %s" s))
  | SyApp (IdIndexed ("tupSel", [ INum i ]), [ arg ]) ->
    let arg' = term_of_sygus env arg in
    mk_sel arg' i
  | SyExists (_, _) -> failwith "Sygus: exists-terms not supported."
  | SyForall (_, _) -> failwith "Sygus: forall-terms not supported."
  (* TODO: add let-conversion. *)
  | SyLet (syg_bindings, syg_term) -> let_bindings_of_sygus env syg_bindings syg_term
  | _ -> failwith "Sygus term not supported."

and let_bindings_of_sygus
    (env : (string, variable, String.comparator_witness) Map.t)
    (bindings : binding list)
    (body : sygus_term)
  =
  let f (bsymb, bterm) =
    (* Create a fresh name and then replace every occurence of the original symbol
       by the new symbol.
    *)
    let varname = Alpha.fresh ~s:bsymb () in
    let bterm' = rename [ bsymb, varname ] bterm in
    let var = Variable.mk varname in
    (bsymb, var), (var, term_of_sygus (Map.set env ~key:varname ~data:var) bterm')
  in
  let subs, t_bindings = List.unzip (List.map ~f bindings) in
  let env' =
    List.fold ~f:(fun env (_, var) -> Map.set env ~key:var.vname ~data:var) ~init:env subs
  in
  Reduce.reduce_term
    (mk_let
       t_bindings
       (term_of_sygus
          env'
          (rename (List.map ~f:(fun (s, var) -> s, var.vname) subs) body)))
;;

(* ============================================================================================= *)
(*                           COMMANDS                                                            *)
(* ============================================================================================= *)

let declare_sort_of_rtype (sname : string) (variants : (string * RType.t list) list)
    : command
  =
  let dt_cons_decs =
    let f (variantname, variantargs) =
      ( variantname
      , List.mapi variantargs ~f:(fun i t ->
            variantname ^ "_" ^ Int.to_string i, sort_of_rtype t) )
    in
    List.map ~f variants
  in
  CDeclareDataType (sname, dt_cons_decs)
;;

let declare_sort_of_tuple (tl : RType.t list) : string * command =
  let name = Tuples.type_name_of_types tl in
  let dt_cons_decs =
    let constr_name = Tuples.constr_name_of_types tl in
    let f i t =
      let proj_name = Tuples.proj_name_of_types tl i in
      proj_name, sort_of_rtype t
    in
    (* A tuple has a single constructor. *)
    [ constr_name, List.mapi ~f tl ]
  in
  name, CDeclareDataType (name, dt_cons_decs)
;;

let declare_sorts_of_var (v : variable) =
  let sort_decls = Map.empty (module String) in
  let rec f sort_decls t =
    RType.(
      match t with
      | TInt | TBool | TChar | TString -> sort_decls
      | TTup tl ->
        let sort_decls' = List.fold ~f ~init:sort_decls tl in
        let tuple_name, tuple_decl = declare_sort_of_tuple tl in
        Map.set sort_decls' ~key:tuple_name ~data:tuple_decl
      | TNamed tname ->
        (match get_variants t with
        | [] -> sort_decls
        | l -> Map.set sort_decls ~key:tname ~data:(declare_sort_of_rtype tname l))
      | _ -> sort_decls)
  in
  Map.to_alist (f sort_decls (Variable.vtype_or_new v))
;;

let sorted_vars_of_types (tl : RType.t list) : sorted_var list =
  let f t =
    (* Declare var for future parsing. *)
    let varname = Alpha.fresh () in
    varname, sort_of_rtype t
  in
  List.map ~f tl
;;

let wait_on_failure (counter : int ref) (t : (solver_response * 'a) Lwt.t)
    : (solver_response * 'a) Lwt.t
  =
  Lwt.bind t (fun t ->
      match t with
      (* Wait on failure. *)
      | (RFail | RUnknown | RInfeasible), _ ->
        Lwt.map
          (fun _ -> t)
          (if !counter > 1
          then (
            Int.decr counter;
            Lwt_unix.sleep !Config.wait_parallel_tlimit)
          else Lwt.return ())
      (* Continue on success. *)
      | _ ->
        Int.decr counter;
        Lwt.return t)
;;

(* ============================================================================================= *)
(*                       ABSTRACTION: SOLVER CLASS                                               *)
(* ============================================================================================= *)

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

  let warning (v : variable) : unit =
    if RType.is_function (Variable.vtype_or_new v)
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

  let define_var (solver : t) (v : variable) : t =
    if Hash_set.mem solver.declared v.vname
    then solver
    else (
      warning v;
      Hash_set.add solver.declared v.vname;
      let sorts = declare_sorts_of_var v in
      let solver = List.fold ~f:declare_sort sorts ~init:solver in
      { solver with
        definitions =
          solver.definitions
          @ [ CDeclareVar (v.vname, sort_of_rtype (Variable.vtype_or_new v)) ]
      })
  ;;

  let synthesize (cl : command list) (solver : t) : t =
    let f solver c =
      match c with
      | CSynthFun (name, _, _, _) | CSynthInv (name, _, _) ->
        Hash_set.add solver.declared name;
        { solver with objs = solver.objs @ [ c ] }
      | _ -> solver
    in
    List.fold ~f ~init:solver cl
  ;;

  let constrain (terms : term list) (solver : t) : t =
    let f solver term =
      let t' = sygus_of_term term in
      let solver = Set.fold ~init:solver ~f:define_var (Analysis.free_variables term) in
      { solver with constraints = solver.constraints @ [ CConstraint t' ] }
    in
    List.fold ~init:solver ~f terms
  ;;

  let set_logic (name : string) (solver : t) : t = { solver with logic = name }

  let all_commands (solver : t) =
    [ CSetLogic solver.logic ]
    @ solver.sorts
    @ solver.extra_defs
    @ solver.objs
    @ List.rev solver.definitions
    @ solver.constraints
    @ [ CCheckSynth ]
  ;;

  let solve (solver : t) =
    let solver_kind =
      if !Config.use_eusolver then SygusSolver.EUSolver else SygusSolver.CVC
    in
    let commands = all_commands solver in
    SygusSolver.solve_commands ~solver_kind commands
  ;;

  let to_file (filename : string) (solver : t) : unit =
    Syguslib.Solvers.commands_to_file (all_commands solver) filename
  ;;
end
