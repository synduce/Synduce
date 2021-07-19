%{
    open Front
    open Lang
    open Lang.RType
%}

%token <string> IDENT
%token <string> CIDENT
%token <string> PIDENT
%token <int> INT
%token ABS
%token AND
%token BOOLSORT
%token COLON
%token COMMA
%token DIV
%token EXCLAMATION
%token EQ
%token FALSE
%token FUN
%token INTSORT
%token IN LET LETPMRS LETAND FUNCTION REC
%token LPAR RPAR
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token LT GT LE GE NEQ
%token MAX
%token MIN
%token MINUS
%token MOD
%token OF
%token OR
%token PLUS
%token QUESTION IF THEN ELSE
%token RIGHTARROW
%token TIMES
%token TRUE
%token TYPE
%token VBAR
%token EQUIV ENSURES DEFINING LEMMA
%token EOF


%nonassoc IN ELSE
%nonassoc QUESTION
%nonassoc COLON


%start <program> main

%%

main: f=list(def); EOF                         { f }


def:
    | TYPE t=typedef                            { TypeDef($loc, t) }
    | LETPMRS p=pmrsdef                         { p }
    | LET f=IDENT args=list(IDENT) LBRACE t=expr RBRACE EQ body=expr
                                                { FunDef($loc, f, args, Some t, body) }
    | LET f=IDENT args=list(IDENT) EQ body=expr
                                                { FunDef($loc, f, args, None, body) }
    | LET REC p=letpmrsdef LBRACKET EQUIV spec=IDENT repr=IDENT RBRACKET
                                                { SyntObjDecl($loc, p , spec, repr) }
    | LET REC p=letpmrsdef                      { p }
    | LEMMA f=IDENT ENSURES t=expr              { EnsuresDef($loc, f, t) }



typedef:
    | param=PIDENT; name=IDENT; EQ t=typeterm0      { TDParametric([param], name, t)}
    | LPAR params=separated_list(COMMA, PIDENT) RPAR name=IDENT; EQ t=typeterm0
                                                    { TDParametric(params, name, t)}
    | name=IDENT EQ t=typeterm0                     { TDSimple(name, t) }


typeterm0:
    | l=separated_nonempty_list(VBAR, typeconstr)   { mk_t_sum $loc l}
    | typefun                                       { $1 }

typeconstr:
    | cname=CIDENT OF t1=typefun                    { mk_t_variant $loc cname  [t1]}
    | cname=CIDENT OF t1=typefun TIMES l=separated_list(TIMES,typefun)
                                                    { mk_t_variant $loc cname  (t1 :: l)}
    | cname=CIDENT                                  { mk_t_variant $loc cname []}


typefun:
    | typefun RIGHTARROW typeapp                    { mk_t_fun $loc $1 $3 }
    | typeapp                                       { $1 }

typeapp:
    | tparam=typeapp tname=typetermb                { mk_t_constr $loc [tparam] tname}
    | tparam=typetermb tname=typetermb              { mk_t_constr $loc [tparam] tname}
    | LPAR t1=typeapp COMMA tparams=separated_nonempty_list(COMMA,typeapp) RPAR tname=typetermb
                                                    { mk_t_constr $loc (t1 :: tparams) tname}
    | typetermb                                     { $1 }


typetermb:
    | INTSORT                                           { mk_t_int $loc }
    | BOOLSORT                                          { mk_t_bool $loc }
    | name=IDENT                                        { mk_t_typ $loc name }
    | param=PIDENT                                      { mk_t_param $loc param }
    | LPAR typeapp RPAR                                 { $2 }


pmrsdef:
    | LPAR p=separated_list(COMMA,IDENT) RPAR n=IDENT args=list(IDENT) EQ b=pbody
                                                        { PMRSDef($loc, p, n, args, None, None, b)}

    | n=IDENT args=list(IDENT) EQ b=pbody               { PMRSDef($loc, [], n, args, None, None, b) }

    | LPAR p=separated_list(COMMA,IDENT) RPAR n=IDENT args=list(IDENT) LBRACE e=expr RBRACE EQ b=pbody
                                                        { PMRSDef($loc, p, n, args, None, Some e, b)}

    | n=IDENT args=list(IDENT) LBRACE e=expr RBRACE EQ b=pbody
                                                        { PMRSDef($loc, [], n, args, None, Some e, b) }


letpmrsdef:
    | defs=separated_list(LETAND, functdef)           { CamlPMRSDef($loc, [], None, None, defs) }
    | defs=separated_list(LETAND, functdef) LBRACKET DEFINING xi=list(IDENT) RBRACKET
                                                        { CamlPMRSDef($loc, xi, None, None, defs) }
    | defs=separated_list(LETAND, functdef) LBRACKET ENSURES invariant=expr RBRACKET
                                                        { CamlPMRSDef($loc, [], None, Some invariant, defs) }

functdef:  n=IDENT args=list(IDENT) EQ rules=function_body       { n, args, rules }

/* Starting vertical bar is optional. */
function_body:
    | FUNCTION VBAR rules=separated_nonempty_list(VBAR, frule)      { PmrsBody($loc, rules) }
    | FUNCTION rules=separated_nonempty_list(VBAR, frule)           { PmrsBody($loc, rules) }
    | expr_body=expr                                                { ExprBody($loc, expr_body) }


frule:
    | lhs=primary_e RIGHTARROW rhs=expr                       { $loc, lhs, rhs }

pbody: separated_list(VBAR, prule)                                 { $1 }


prule: args=fun_app_e RIGHTARROW t=expr                             { $loc, args, t }


expr:
    | FUN args=list(primary_e) RIGHTARROW t=tuple_e                 { mk_fun $loc args t }
    | LET arg=primary_e EQ t=expr IN b = expr                       { mk_let $loc arg t b }
    | LET arg1=primary_e COMMA rest=separated_nonempty_list(COMMA, primary_e) EQ t=expr IN b=expr
                                                                    { mk_let $loc (mk_tup $loc (arg1::rest)) t b }
    | c=expr QUESTION t=expr; COLON f=expr                          { mk_ite $loc c t f}
    | IF c=expr THEN t1=expr ELSE t2=expr                           { mk_ite $loc c t1 t2}
    | tuple_e                                                       { $1 }

tuple_e:
    | t1=logical_or_e COMMA tl=separated_nonempty_list(COMMA, logical_or_e)
                                                                    { mk_tup $loc (t1 :: tl) }
    | logical_or_e                                                  { $1 }

logical_or_e:
    | a=logical_and_e OR b=logical_or_e                             { mk_bin $loc Term.Binop.Or a b}
    | logical_and_e                                                 { $1 }

logical_and_e:
    | a=equality_e AND b=logical_and_e                              { mk_bin $loc Term.Binop.And a b}
    | equality_e                                                    { $1 }

equality_e:
    | a=comp_e EQ b=comp_e                                          { mk_bin $loc Term.Binop.Eq a b }
    | a=comp_e NEQ b=comp_e                                         { mk_un $loc Term.Unop.Not
                                                                        (mk_bin $loc Term.Binop.Eq a b) }
    | comp_e                                                        { $1 }

comp_e:
    | a=add_e op=op_comp b=add_e                                    { mk_bin $loc op a b }
    | add_e                                                         { $1 }

add_e:
    | a=mult_e op=op_add b=add_e                                    { mk_bin $loc op a b }
    | mult_e                                                        { $1 }

mult_e:
    | a=unary_e op=op_mult b=mult_e                                 { mk_bin $loc op a b }
    | unary_e                                                       { $1 }

unary_e:
    | op=unop a=unary_e                                             { mk_un $loc op a }
    | fun_app_e                                                     { $1 }



fun_app_e:
    | f=primary_e arg=nonempty_list(primary_e)                      { mk_app $loc f arg }
    | MAX a=primary_e b=primary_e                                   { mk_bin $loc Term.Binop.Max a b}
    | MIN a=primary_e b=primary_e                                   { mk_bin $loc Term.Binop.Min a b}
    | primary_e                                                     { $1 }


primary_e:
    | CIDENT LPAR l=separated_list(COMMA, fun_app_e) RPAR           { mk_data $loc $1 l  }
    | CIDENT                                                        { mk_data $loc $1 [] }
    | LPAR t=expr RPAR                                              { t }
    | v=IDENT                                                       { mk_var $loc v }
    | TRUE                                                          { mk_const $loc Term.Constant.CTrue}
    | FALSE                                                         { mk_const $loc Term.Constant.CFalse}
    | i=INT                                                         { mk_const $loc (Term.Constant.of_int i)}


%inline op_comp:
    | LT        { Term.Binop.Lt }
    | GT        { Term.Binop.Gt }
    | LE        { Term.Binop.Le }
    | GE        { Term.Binop.Ge }

%inline op_add:
    | PLUS      { Term.Binop.Plus }
    | MINUS     { Term.Binop.Minus }

%inline op_mult:
    | DIV       { Term.Binop.Div }
    | TIMES     { Term.Binop.Times }
    | MOD       { Term.Binop.Mod }

%inline unop:
    | MINUS       { Term.Unop.Neg }
    | EXCLAMATION { Term.Unop.Not }
    | ABS         { Term.Unop.Abs }
