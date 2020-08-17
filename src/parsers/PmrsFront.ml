open Base
open Utils
open Front

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
  let _ : (string, Lang.Term.variable) Hashtbl.t = Hashtbl.create (module String) in
  let _getglob decl =
    match decl with
    | FunDecl (_, _, _, _) -> ()
    | PMRSDecl(_, _, _, _, _) -> ()
    | _ -> ()
  in
  List.iter ~f:_getglob prog