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

%start <MiniooAbstractSyntax.configuration> prog                   /* the entry point */

%type < ast > assign
%type < ast > decl
%type < ast > seqctrl
%type < ast > cmd
%type < ast > bool
%type < ast > expr
%left MINUS          /* lowest precedence  */

%% /* rules */

prog :
    cmd EOF { $1 }
	
cmd :
    decl        { $1 }    
  | assign      { $1 }
  | seqctrl     { $1 }
  | malloc      { $1 }
  | reccall     { $1 }
  | atom        { $1 }
  | parallel    { $1 }

(* how to write assign with poitner now? a: this will be the heap allocation *) 
 
decl :
    VAR x = IDENT SEMICOLON cmd { Decl(x, $4) }

assign :
    x = IDENT ASSIGN e = expr  { Assign(x , e) }
  | e1 = expr LOCATION e2 = expr ASSIGN e3 = expr { FieldAssign(e1, e2, e3) }

seqctrl :
    SKIP                          { Skip }
  | LCUR cmd SEMICOLON cmd RCUR   { Seq($2, $4) }
  | WHILE bool cmd                { While($2, $3) }
  | IF bool cmd ELSE cmd          { If($2, $3, $5) }
	
malloc :
    MALLOC LPAREN x = IDENT RPAREN { Malloc(x) }
  
reccall : 
    e1 = expr LPAREN e2 = expr RPAREN { RecProcCall(e1, e2) }

atom :
    ATOM LPAREN c = cmd RPAREN    { Atom(c) }

parallel :
    LCUR c1 = cmd PARALLEL c2 = cmd RCUR { Parallel(c1, c2) }

expr :
    f = FIELD                     { Field (f) }      /* field expression */
  | e1 = expr MINUS e2 = expr     { Diff(e1, e2) }  /* arithmetic expression */
  | v = NUM                       { Num v }
  | v = IDENT                     { Ident (v, Void) }        /* location expression */
  | NULL                          { Null }
  | e1 = expr LOCATION e2 = expr  { Loc(e1, e2) }
  | PROCEDURE y=IDENT COLON cmd   { Proc(y, $4) } /* recursive procedure expression */


bool :
    TRUE                          { True }
  | FALSE                         { False }
  | e1 = expr EQUALS e2 = expr    { Equal(e1, e2) }  
  | e1 = expr LT e2 = expr        { LessT(e1, e2) }

%% (* trailer *)
