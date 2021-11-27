(* File minioo.ml *)
open Parsing;;
open MiniooAbstractSyntax;;
open MiniooMENHIR;;

let a_string = "a string" in
  Printf.printf "Debug: %s\n" a_string; 
 
let lexbuf = Lexing.from_channel stdin in
  try
    let ast = MiniooMENHIR.prog MiniooLEX.token lexbuf in 
    let l = 0 in 
    ptr_cmd ast l ([]:MiniooAbstractSyntax.stack) ([]:MiniooAbstractSyntax.heap ); flush stdout
  with Parse_error ->
    (failwith "incorrect pasing") ;

;;