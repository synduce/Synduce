open Base
open Utils
open Front
open Lang.Term

module P = Pmrs_parser
module L = Pmrs_lexer

exception SyntaxError of string

let verbose = ref true
let text = ref ""

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
          let vars = List.map ~f:Variable.mk args in
          ()
        | _ -> ()
      )
