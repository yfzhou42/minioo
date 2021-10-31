
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TRUE
  | SKIP
  | SEMICOLON
  | RPAREN
  | RCUR
  | PROCEDURE
  | PARALLEL
  | NUM of ( int )
  | NULL
  | MINUS
  | MALLOC
  | LT
  | LPAREN
  | LOCATION
  | LCUR
  | IF
  | IDENT of ( string )
  | FIELD of ( string )
  | FALSE
  | EQUALS
  | EOF
  | ELSE
  | COLON
  | ATOM
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (MiniooAbstractSyntax.cmdAST)
