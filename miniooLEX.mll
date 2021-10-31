(* File miniooMENHIR.mll *)
{
open MiniooMENHIR  (* Type token defined in miniooMENHIR.mli *)
exception Eof

}
rule token = parse
    [' ' '\t' '\n' '\r'] { print_string "_"; token lexbuf } (* skip blanks and tabs *)

  | "true"      as bool { TRUE }
  | "false"     as bool { FALSE }

  | "var"      { print_string "var"; VAR }
  | "proc"     { PROCEDURE }
  | "null"     { NULL }
  | "if"       { IF }
  | "else"     { ELSE }
  | "while"    { WHILE }
  | "malloc"   { MALLOC }
  | "skip"     { SKIP }
  | "atom"     { ATOM }
  | "malloc"   { MALLOC }
  | "eof"      { print_string "eof"; EOF }
  | (['a'-'z'] | ['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as ident
               { IDENT ident }
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


