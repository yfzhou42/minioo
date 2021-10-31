%{ (* header *)
  
open MiniooAbstractSyntax

%} 
/* declarations */

%token SEMICOLON ASSIGN EOF /* lexer tokens */
%token MINUS LPAREN RPAREN LCUR RCUR
%token NULL WHILE IF ELSE EQUALS LT MALLOC SKIP ATOM PARALLEL TRUE FALSE COLON PROCEDURE LOCATION VAR


%token < string > IDENT
%token < string > FIELD
%token < int > NUM

%start <MiniooAbstractSyntax.cmdAST>prog                   /* the entry point */
%type < cmdAST > assign
%type < cmdAST > decl
%type < cmdAST > seqctrl
%type < cmdAST > cmd
%type < boolEXPR > bool
%type < exprAST > expr
%left MINUS          /* lowest precedence  */

%% /* rules */

prog :
    cmd EOF { $1 }
	
cmd :
    assign      { $1 }
  | decl        { $1 }
  | seqctrl     { $1 }
  | malloc      { $1 }
  
assign :
    x = IDENT ASSIGN e = expr  { Assign(x, e) }
  
decl :
    VAR x = IDENT SEMICOLON cmd { Decl(x, $4) }

malloc :
    MALLOC LPAREN x = IDENT RPAREN { Malloc(x) }
  
seqctrl :
    SKIP                          { Skip }
  | LCUR cmd SEMICOLON cmd RCUR   { Seq($2, $4) }
  | WHILE bool cmd                { While($2, $3) }
  | IF bool cmd ELSE cmd          { If($2, $3, $5) }
	
expr :
    f = FIELD                     { Field(f) }      /* field expression */
  | e1 = expr MINUS e2 = expr     { Diff(e1, e2) }  /* arithmetic expression */
  | v = NUM                       { Num v }
  | v = IDENT                     { Ident v }        /* location expression */
  | NULL                          { Null }
  | e1 = expr LOCATION e2 = expr  { Loc(e1, e2) }
  | PROCEDURE IDENT COLON cmd     { Proc($2, $4) } /* recursive procedure expression */


bool :
    TRUE                          { True }
  | FALSE                         { False }
  | e1 = expr EQUALS e2 = expr    { Equal(e1, e2) }  
  | e1 = expr LT e2 = expr        { LessT(e1, e2) }

%% (* trailer *)
