open Base
open Utils
open Front
open Lang

module P = Pmrs_parser
module L = Pmrs_lexer

exception SyntaxError of string

let verbose = ref true
let text = ref ""

let loc_fatal_errmsg loc msg =
  Log.(error (fun f () -> log_with_excerpt f !text loc Fmt.string msg); fatal ())

(* Some fatal error that print information. *)
let variable_not_found loc k =
  Log.(error
         (fun f () -> log_with_excerpt f !text loc
             (fun ff s -> Fmt.(pf ff "%s undefined." s)) k));
  Log.fatal ()


let parsefile filename =
  (* Save the text for better error reporting. *)
  text := Stdio.In_channel.read_all filename;
  let lexbuf = Lexing.from_channel ~with_positions:true (Stdio.In_channel.create filename) in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename;};
  let module I = P.MenhirInterpreter in
  let checkpoint = P.Incremental.main lexbuf.lex_curr_p
  and supplier = I.lexer_lexbuf_to_supplier L.token lexbuf
  and succeed cp = cp
  and fail checkpoint =
    Log.(error (wrap1 "%a" Fmt.string (ErrorReports.report !text checkpoint)));
    []
  in
  I.loop_handle succeed fail supplier checkpoint

(**
   Iterates through the toplevel declarations of the program and adds the type declaration
   to the global type environment in Lang.RType.
*)
let seek_types (prog : program) =
  List.iter
    ~f:(fun decl ->
        match decl with
        | TypeDecl(_, TDParametric(p, typename, term)) ->
          (match Lang.RType.add_type ~params:p ~typename term with
           | Ok _ -> ()
           | Error es ->
             Log.(error (fun f () -> log_with_excerpt f !text term.pos Sexp.pp_hum es));
             Log.fatal ())

        | TypeDecl(_, TDSimple(typename, term)) ->
          (match Lang.RType.add_type ~typename term with
           | Ok _ -> ()
           | Error es ->
             Log.(error (fun f () -> log_with_excerpt f !text term.pos Sexp.pp_hum es));
             Log.fatal ())

        | _ -> ())
    prog

let fterm_to_term rloc allv globs locs rterm =
  let findv loc env k =
    match Map.find env k with
    | None ->
      (match Term.VarSet.find_by_name allv k with
       | None ->
         (match Hashtbl.find globs k with
          | None -> variable_not_found loc k
          | Some x -> x)
       | Some x -> x)
    | Some x -> x
  in
  let _env =
    match Term.VarSet.to_env locs with
    | `Ok env -> env
    | `Duplicate_key s -> loc_fatal_errmsg rloc ("Rule has duplicate variable "^s)
  in
  let rec f env t =
    match t.kind with
    | FTConst c -> Term.mk_const ~pos:t.pos c
    | FTApp (func, args) -> Term.(mk_app ~pos:t.pos (f env func) (List.map ~f:(f env) args))
    | FTData (constr, args) -> Term.(mk_data ~pos:t.pos constr (List.map ~f:(f env) args))
    | FTVar v -> Term.mk_var (findv t.pos env v)
    | FTTup l ->  Term.(mk_tup ~pos:t.pos (List.map ~f:(f env) l))
    | FTFun (args, body) ->
      let fargs = List.map ~f:Term.Variable.mk args in
      let new_env =
        List.fold ~init:env fargs
          ~f:(fun accum_env v ->
              if Map.mem accum_env v.vname
              then Map.change accum_env v.vname ~f:(fun _ -> Some v)
              else Map.add_exn accum_env ~key:v.vname ~data:v)
      in Term.(mk_fun ~pos:t.pos fargs (f new_env body))
    | FTBin (op, t1, t2) -> Term.(mk_bin ~pos:t.pos op (f env t1) (f env t2))
    | FTUn (op, t1) -> Term.(mk_un ~pos:t.pos op (f env t1))
    | FTIte (c, a, b) -> Term.(mk_ite ~pos:t.pos (f env c) (f env a) (f env b))
    | FTHOBin _ -> failwith "<case>"
  in
  f _env rterm

let translate_rules loc (globs : (string, Term.variable) Hashtbl.t)
    (params : Term.variable list) (args : Term.variable list)
    (pname : string)
    (body : pmrs_body)
  : PMRScheme.pmrs =
  (* Check that params and args do not have variables with the same name.
     Params and args can shadow globals though.
  *)
  List.iter
    ~f:(fun vp ->
        if List.mem args vp ~equal:Term.Variable.same_name then
          loc_fatal_errmsg loc "Duplicate parameter and argument name:")
    params;
  let pset = Term.VarSet.of_list params and aset= Term.VarSet.of_list args in
  (* First pass to collect the non-terminal variables. *)
  let nont =
    let f accum (rloc, rhead, _ ) =
      match rhead.kind with
      | FTApp(func, _) ->
        (match func.kind with
         | FTVar nt ->
           (match Term.VarSet.find_by_name accum nt with
            | Some _ -> accum
            | None -> Set.add accum (Term.Variable.mk ~attrs:Term.Attributes.(singleton (NonTerminal 0)) nt))
         | _ ->
           loc_fatal_errmsg rloc (Fmt.str "Rule head first term should be a variable, not %a." pp_fterm func)
        )
      | _ -> loc_fatal_errmsg rloc (Fmt.str "Rule head should be an applicative term, not %a." pp_fterm rhead)
    in
    List.fold ~f ~init:Term.VarSet.empty body
  in
  (* Find a variable in environment. If it's not in the local enviroment, it should be in the globals.*)
  let allv = Term.VarSet.union_list [nont; pset; aset] in
  let translate_pattern constr args : PMRScheme.pattern =
    let f arg =
      match arg.kind with
      | FTVar x -> Term.Variable.mk x
      | _ -> loc_fatal_errmsg arg.pos (Fmt.str "expected a variable in pattern, got %a" pp_fterm arg)
    in constr, List.map ~f args
  in
  let rules : PMRScheme.rewrite_rule list =
    let transf_rule (rloc, rhead, rterm : pmrs_rule) =
      let nt, (xs, pat) =
        let rec f r_args =
          match r_args with
          | [{kind = FTVar xt; _}] -> [Term.Variable.mk xt], None
          | [{kind = FTData(constr, args); _}] -> [], Some (translate_pattern constr args)
          | ({kind = FTVar xt; _}) :: rest -> let s, p = f rest in (Term.Variable.mk xt) :: s, p
          | [] -> [], None
          | hd :: _ -> loc_fatal_errmsg hd.pos "Unexpected term in rule head."
        in
        match rhead.kind with
        | FTApp(rf, rule_args) ->
          (match rf.kind with
           | FTVar nt ->
             (match Term.VarSet.find_by_name nont nt with
              | Some x -> x
              (* The cases below should have been filtered when seeking non-terminals. *)
              | None -> failwith "impossible")
           | _ -> failwith "impossible"),
          f rule_args

        | _ -> loc_fatal_errmsg rloc "Rule head is empty."
      in
      let local_vars : Term.VarSet.t =
        let pat_vars =
          match pat with
          | Some (_, xs) -> Term.VarSet.of_list xs
          | None -> Term.VarSet.empty
        in
        Set.union (Term.VarSet.of_list xs) pat_vars
      in
      let rule_rhs = fterm_to_term rloc allv globs local_vars rterm in
      nt, xs, pat, rule_rhs
    in
    List.map ~f:transf_rule body
  in
  Term.{
    pname = pname;
    pargs = VarSet.of_list args;
    pparams = VarSet.of_list params;
    pnon_terminals = nont;
    prules = rules;
    porder = -1;
    pmain_id = -1;
  }



let translate (prog : program) : PMRScheme.pmrs list =
  let globals : (string, Term.variable) Hashtbl.t = Hashtbl.create (module String) in
  (* First pass to create the global variables *)
  List.iter prog
    ~f:(fun decl ->
        match decl with
        | FunDecl (loc, fname, _,_)
        | PMRSDecl(loc, _, fname, _, _) ->
          (match Hashtbl.add globals ~key:fname ~data:(Term.Variable.mk fname) with
           | `Ok -> ()
           | `Duplicate ->
             loc_fatal_errmsg loc (Fmt.str "%s already declared." fname))
        | _ -> ());
  (* Second pass  *)
  List.fold ~init:[] prog
    ~f:(fun l decl ->
        match decl with
        | PMRSDecl(loc, params, pname, args, body) ->
          let vparams = List.map ~f:Term.Variable.mk params in
          let vargs = List.map ~f:Term.Variable.mk args in
          l @ [translate_rules loc globals vparams vargs pname body]

        | _ -> l)
