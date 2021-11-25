{
open Pmrs_parser
open Lexing
exception LexError of string

exception SyntaxError of string

let keywords =
    [
        "@ensures", ENSURES;
        "@equiv", EQUIV;
        "@defining", DEFINING;
        "abs", ABS;
        "and", LETAND;
        "boolean", BOOLSORT;
        "else", ELSE;
        "false", FALSE;
        "fun", FUN;
        "function", FUNCTION;
        "in", IN;
        "if", IF;
        "int", INTSORT;
        "let", LET;
        "lemma", LEMMA;
        "pmrs", LETPMRS;
        "max", MAX;
        "min", MIN;
        "of", OF;
        "rec", REC;
        "then", THEN;
        "type", TYPE;
        "true", TRUE
    ]

let keyword_tbl = Hashtbl.create 256
let uncurry f (a, b) = f a b
let _ = List.iter (uncurry (Hashtbl.replace keyword_tbl)) keywords

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let id = ['_' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let capid = ['A'-'Z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let primed_id = ['''] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let at_id = ['@' '#'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let nl = ['\n' '\r']
let ws = ['\n' '\t' '\r' ' ']
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let int = '-'? ['0'-'9'] ['0'-'9']*

rule token = parse
  | primed_id as pid  { PIDENT pid }
  | capid as capid    { CIDENT capid }
  | id as id          { try Hashtbl.find keyword_tbl id with Not_found -> IDENT id }
  | at_id as keyw      { try Hashtbl.find keyword_tbl keyw with
                            Not_found -> raise (LexError ("Unexpected keyword "^keyw)) }
  | "//"              { comment lexbuf }
  | "(*"              { multi_line_comment lexbuf }
  | "->"              { RIGHTARROW }
  | "!="              { NEQ }
  | "&&"              { AND }
  | "||"              { OR }
  | "<"               { LT }
  | "<="              { LE }
  | ">"               { GT }
  | ">="              { GE }
  | ":"               { COLON }
  | ","               { COMMA }
  | "("               { LPAR }
  | ")"               { RPAR }
  | "{"               { LBRACE }
  | "}"               { RBRACE }
  | "["               { LBRACKET }
  | "]"               { RBRACKET }
  | "+"               { PLUS }
  | "-"               { MINUS }
  | "*"               { TIMES }
  | "/"               { DIV }
  | "!"               { EXCLAMATION }
  | "|"               { VBAR }
  | "%"               { MOD }
  | "="               { EQ }
  | "?"               { QUESTION }
  | int as int        { INT (int_of_string int) }
  | nl                { next_line lexbuf; token lexbuf }
  | ws                { token lexbuf }
  | eof               { EOF }
  | _                 { raise (LexError ("Unexpected char: "^(Lexing.lexeme lexbuf))) }

and string = parse
    "\\\\"           { "\\" :: (string lexbuf) }
  | "\\\""           { "\"" :: (string lexbuf) }
  | "\\n"            { "\n" :: (string lexbuf) }
  | "\\t"            { "\t" :: (string lexbuf) }
  | "\""             { [] }
  | _ as c           { (String.make 1 c) :: (string lexbuf) }

(* comments *)
and comment = parse
    nl               { next_line lexbuf; token lexbuf }
  | eof              { EOF }
  | _                { comment lexbuf }

and multi_line_comment = parse
  | "*)" { token lexbuf }
  | nl { next_line lexbuf; multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Unexpected EOF - unterminated comment.")) }
  | _ { multi_line_comment lexbuf }