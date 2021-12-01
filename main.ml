(* File minioo.ml *)
open Parsing;;
open MiniooAbstractSyntax;;
open MiniooMENHIR;;

(* read stdin with lexbuf = Lexing.from_channel stdin  *)
(* lex with lexed = MiniooLEX.token lexbuf *)
(* parse with ast = MiniooMENHIR.prog lexed *)

(* linking with linkedast = link_ast ast  *)
(* trace with trace linked_ast *)

let lexbuf = Lexing.from_channel stdin in
  try
    print_string "\nI am in lexbuf\n";
    let ast = MiniooMENHIR.prog MiniooLEX.token lexbuf in 
    (* let linked_ast = link_ast ast in
    print_linked_ast linked_ast; *)
    print_ast ast; (print_string "\n end of code\n ");
    (print_string "\n!!!Enter Interpreter!!!\n");
    trace ast ([]:MiniooAbstractSyntax.stack) ([]:MiniooAbstractSyntax.heap);  flush stdout
  with Parse_error ->
    (failwith "incorrect pasing") ;

;;