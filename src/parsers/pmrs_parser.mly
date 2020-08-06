%{
    open Front
    open Lang.Term
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
%token INTSORT
%token LET LETPMRS
%token LPAR RPAR
%token LT GT LE GE NEQ
%token MAX
%token MIN
%token MINUS
%token MOD
%token OF
%token OR
%token PLUS
%token QUESTION
%token RIGHTARROW
%token TIMES
%token TRUE
%token TYPE
%token VBAR
%token EOF

%nonassoc QUESTION
%nonassoc COLON


%start <program> main

%%

main: f=list(decl); EOF                                                             { f }


decl:
    | TYPE t=typedecl                                                               { TypeDecl(t) }
    | LETPMRS p=pmrsdecl                                                            { PMRSDecl(p) }
    | LET f=IDENT args=list(IDENT) EQ body=expr                                     { FunDecl(f, args, body) }


typedecl:
    | param=PIDENT; name=IDENT; EQ t=typeterm0                                       { TDParametric([param], name, t)}
    | LPAR params=separated_list(COMMA, PIDENT) RPAR name=IDENT; EQ t=typeterm0      { TDParametric(params, name, t)}
    | name=IDENT EQ t=typeterm0                                                      { TDSimple(name, t) }


typeterm0:
    | l=separated_nonempty_list(VBAR, typeconstr)                                     { TSum(l)}
    | typeapp                                                                        { $1 }

typeconstr:
    | cname=CIDENT OF t1=typeapp                                                   { TConstr(cname, [t1])}
    | cname=CIDENT OF t1=typeapp TIMES l=separated_list(TIMES,typeapp)           { TConstr(cname, t1 :: l)}
    | cname=CIDENT                                                                   { TConstr(cname,[])}

typeapp:
    | tparam=typeapp tname=typetermb                                                { TParamTyp([tparam],tname)}
    | tparam=typetermb tname=typetermb                                              { TParamTyp([tparam],tname)}
    | LPAR t1=typeapp COMMA tparams=separated_nonempty_list(COMMA,typeapp) RPAR tname=typetermb
                                                                                    { TParamTyp(t1 :: tparams, tname)}
    | typetermb                                                                     { $1 }


typetermb:
    | INTSORT                                                                       { TInt }
    | BOOLSORT                                                                      { TBool }
    | name=IDENT                                                                    { TTyp(name) }
    | param=PIDENT                                                                  { TParam(param)}
    | LPAR typeapp RPAR                                                            { $2 }


pmrsdecl:
    | LPAR p=separated_list(COMMA,IDENT) RPAR n=IDENT args=list(IDENT) EQ b=pbody   { p, n, args, b}
    | n=IDENT args=list(IDENT) EQ b=pbody   { [], n, args, b}


pbody: separated_list(VBAR, prule)                                                    { $1 }


prule: args=list(constr_e) RIGHTARROW t=expr                                    { args, t }


expr:
    | c=expr QUESTION t=expr; COLON f=expr                                      { mk_ite $loc c t f}
    | logical_or_e                                                              { $1 }

logical_or_e:
    | a=logical_and_e OR b=logical_or_e                                         { mk_bin $loc Binop.Or a b}
    | logical_and_e                                                             { $1 }

logical_and_e:
    | a=equality_e AND b=logical_and_e                                          { mk_bin $loc Binop.And a b}
    | equality_e                                                                { $1 }

equality_e:
    | a=comp_e EQ b=comp_e                                                      { mk_bin $loc Binop.Eq a b }
    | a=comp_e NEQ b=comp_e                                                     { mk_bin $loc Binop.Neq a b }
    | comp_e                                                                    { $1 }

comp_e:
    | a=add_e op=op_comp b=add_e                                                { mk_bin $loc op a b }
    | add_e                                                                     { $1 }

add_e:
    | a=mult_e op=op_add b=add_e                                                { mk_bin $loc op a b }
    | mult_e                                                                    { $1 }

mult_e:
    | a=unary_e op=op_mult b=mult_e                                             { mk_bin $loc op a b }
    | unary_e                                                                   { $1 }

unary_e:
    | op=unop a=unary_e                                                         { mk_un $loc op a }
    | constr_e                                                                  { $1 }

constr_e:
    | CIDENT LPAR l=separated_list(COMMA, expr) RPAR                            { mk_data $loc $1 l  }
    | CIDENT                                                                    { mk_data $loc $1 [] }
    | fun_app_e                                                                 { $1 }

fun_app_e:
    | f=fun_app_e arg=primary_e                                                 { mk_app $loc f arg }
    | f=primary_e arg=primary_e                                                 { mk_app $loc f arg }
    | MAX a=primary_e b=primary_e                                               { mk_bin $loc Binop.Max a b}
    | MIN a=primary_e b=primary_e                                               { mk_bin $loc Binop.Min a b}
    | primary_e                                                                 { $1 }

primary_e:
    | LPAR t=expr RPAR                                                          { t }
    | v=IDENT                                                                   { mk_var $loc v }
    | TRUE                                                                      { mk_const $loc Constant.CTrue}
    | FALSE                                                                     { mk_const $loc Constant.CFalse}
    | i=INT                                                                     { mk_const $loc (Constant.of_int i)}

%inline op_comp:
    | LT        { Binop.Lt }
    | GT        { Binop.Gt }
    | LE        { Binop.Le }
    | GE        { Binop.Ge }

%inline op_add:
    | PLUS      { Binop.Plus }
    | MINUS     { Binop.Minus }

%inline op_mult:
    | DIV       { Binop.Div }
    | TIMES     { Binop.Times }
    | MOD       { Binop.Mod }

%inline unop:
    | MINUS       { Unop.Neg }
    | EXCLAMATION { Unop.Not }
    | ABS         { Unop.Abs }
