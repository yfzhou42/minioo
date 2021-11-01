(* File miniooAbstractSyntax.ml *)
type ptr_decl = 
  Decl_ptr of cmdAST
  |Proc_ptr of exprAST
  |Void


type boolEXPR = 
  True 
  | False
  | Equal of exprAST * exprAST 
  | LessT of exprAST * exprAST 

and exprAST =
  Ident of string * ptr_decl
  | Field of string 
  | Num of int
  | Diff of exprAST * exprAST 
  | Null 
  | Proc of string * cmdAST 
  | Loc of exprAST * exprAST 
  
and cmdAST = 
  Decl of string * cmdAST 
  | Assign of string * exprAST * ptr_decl
  | Seq of cmdAST * cmdAST
  | Skip
  | If of boolEXPR * cmdAST * cmdAST 
  | While of boolEXPR * cmdAST 
  | Malloc of string 
;;

let rec ptr_cmd t st = 
  match t with 
    Decl (s, c) -> ptr_cmd c (s, Decl_ptr t)::st
    | Proc (s, c) -> ptr_cmd c (s, Proc_ptr t)::st

let rec print_exprast expr = 
  match expr with 

    Ident s -> print_string (s) ;print_newline();
    |Num num -> print_string (string_of_int num) ;print_newline();
    | _  -> failwith "print_ast";;

let rec print_cmdast cmd = 
  match cmd with 

    Decl (s, cmdast) -> print_string ("Declaration:"^s) ;print_newline();
                         print_string ("Command List:") ;print_newline();
                         print_cmdast cmdast; print_newline()
    |Assign (s, exprast) -> print_string ("Assign: "^s) ;print_newline();
                            print_string (" = ") ;print_newline();
                            print_exprast exprast; print_newline()
    | _  -> failwith "print_ast";;

