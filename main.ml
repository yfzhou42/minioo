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
    let ast = MiniooMENHIR.prog MiniooLEX.token lexbuf in 
    (* let linked_ast = link_ast ast in
    print_linked_ast linked_ast; *)
    print_ast ast;
    trace ast ([]:MiniooAbstractSyntax.stack) ([]:MiniooAbstractSyntax.heap);  flush stdout
  with Parse_error ->
    (failwith "incorrect pasing") ;

;;