(* File miniooMENHIR.mll *)
{
open MiniooMENHIR  (* Type token defined in miniooMENHIR.mli *)
exception Eof

}
rule token = parse
    [' ' '\t' '\n' '\r'] {token lexbuf } (* skip blanks and tabs *)

  | "true"      as bool { TRUE }
  | "false"     as bool { FALSE }

  | "var"      { VAR }
  | "proc"     { PROCEDURE }
  | "null"     { NULL }
  | "if"       { IF }
  | "else"     { ELSE }
  | "while"    { WHILE }
  | "malloc"   { MALLOC }
  | "skip"     { SKIP }
  | "atom"     { ATOM }
  | "eof"      { print_string "\n lexer reached end of file \n"; EOF }
  | (['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as ident
               { IDENT ident }
  | (['a'-'z'] )(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as field
               { FIELD field }
               (*field with start with lower case*)
  | ['0'-'9']+ as num
               { NUM (int_of_string num) }   
  | "|||"      { PARALLEL }

  | "=="       { EQUALS }
  | '<'        { LT }
  | ':'        { COLON }

  | ';'        { SEMICOLON }
  | '='        { ASSIGN }
  | '-'        { MINUS }   
  | '('        { LPAREN }
  | ')'        { RPAREN }
  
  | '.'        { LOCATION }
  | '{'        { LCUR }
  | '}'        { RCUR }


