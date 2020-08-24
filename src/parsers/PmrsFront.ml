open Base
open Utils
open Front
open Lang.Term
open Lang.PMRScheme

module P = Pmrs_parser
module L = Pmrs_lexer

exception SyntaxError of string

let verbose = ref true
let text = ref ""

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


let translate_rules loc (globs : (string, variable) Hashtbl.t)
    (params : variable list) (args : variable list)
    (body : pmrs_body)
  : pmrs =
  (* Check that params and args do not have variables with the same name.
     Params and args can shadow globals though.
  *)
  List.iter
    ~f:(fun vp ->
        if List.mem args vp ~equal:Variable.same_name then
          (Log.(error
                  (fun f () -> log_with_excerpt f !text loc Fmt.string "Duplicate parameter and argument name:"));
           Log.fatal ()))
    params;
  let pset = VarSet.of_list params and aset= VarSet.of_list args in
  (* First pass to collect the non-terminal variables. *)
  let nont =
    let f accum x =
      match x with 
      |
    in
    List.fold ~f ~init:VarSet.empty body
  in
  (* Find a variable in environment. If it's not in the local enviroment, it should be in the globals.*)
  let allv = VarSet.union_list [nont; pset; aset] in
  let findv loc env k =
    Option.value (Map.find env k)
      ~default:(Option.value (VarSet.find_by_name allv k)
                  ~default:(Option.value (Hashtbl.find globs k)
                              ~default:(variable_not_found loc k)))
  in



  let translate (prog : program) =
    let globals : (string, variable) Hashtbl.t = Hashtbl.create (module String) in
    (* First pass to create the global variables *)
    List.iter prog
      ~f:(fun decl ->
          match decl with
          | FunDecl (loc, fname, _,_)
          | PMRSDecl(loc, _, fname, _, _) ->
            (match Hashtbl.add globals ~key:fname ~data:(Variable.mk fname) with
             | `Ok -> ()
             | `Duplicate ->
               Log.(error (fun f () -> log_with_excerpt f !text loc
                              (fun f () -> Fmt.(pf f "%s already declared." fname)) ()));
               Log.fatal ())

          | _ -> ());
    (* Second pass  *)
    List.iter prog
      ~f:(fun decl ->
          match decl with
          | PMRSDecl(loc, params, fname, args, body) ->
            let vparams = List.map ~f:Variable.mk params in
            let vargs = List.map ~f:Variable.mk args in
            let _ = translate_rules loc globals vparams vargs body in
            ()
          | _ -> ()
        )
