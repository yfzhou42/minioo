(* File miniooAbstractSyntax.ml *)

type ast =
  Ident of string * ptr_decl (**check the delcaration*)
  | Field of string 
  | Num of int
  | Diff of ast * ast 
  | Null 
  | Proc of string * ast
  | Loc of ast * ast

  | Decl of string * ast
  | Assign of string * ptr_decl * ast
  | FieldAssign of ast * ast * ast 
  | Seq of ast * ast
  | Skip
  | If of ast * ast * ast
  | While of ast * ast
  | Malloc of ast
  | RecProcCall of ast * ast 
  | Atom of ast
  | Parallel of ast * ast 
  | Empty

  | True 
  | False
  | Equal of ast * ast 
  | LessT of ast * ast 

  | Block of ast

and ptr_decl = 
  Ptr of ast
  | Void
;; 

(* syntax and semantics *)

type boolean = 
  BoolT 
  |BoolF 
  |BoolErr 
;;

type obj = 
  Obj of int
and location = 
  Locat of obj
  |LNull 
;;

type stack = (ast * location) list
;;

type closure = 
  Clo of ast * ast * stack
;;

type tva = 
  TvaError 
  | TvaField of ast
  | TvaInt of int
  | TvaLoc of location
  | TvaClo of closure
  | TvaNull of ast
;;
  
type heapentry =  
  Entry of (ast * tva ref) list 
and heap = heapentry list 
;;

type configuration = 
  Conf of ast * stack * heap
;;

(* I need to have a gloable reference table? it will be a *)
(* ref (string * prt_decl) list *)


(* push to stack at Decl and Proc *)
(* s is string, t is the Decl/Proc ast, l is location correpsonds to heap, st is the sack*)
(* string -> ast -> int -> stack -> stack  *)
(** second thought, bad decision, now make parameter be the more general ast_s, so stack can also store fields*)
(** ast -> int -> stack -> stack *)
let push_stack ast_s l st = 
  (ast_s, Locat(Obj(l))) :: st
;;

(* stack -> stack *)
let pop_stack st =
  match st with 
  [] -> failwith "pop on empty stack"
  |h::t -> t 
;;

(* search the stack st for variable ast_s and return the location corresponding to ast_s for heap val storation *) 
(* ast_s here can only be Ident(s, ptr) *)
(* ast -> stack -> location *) 
let get_loc_from_stack ast_s st =
  if List.mem_assoc ast_s st then List.assoc ast_s st
  else failwith "the varible is not stored on stack"
;;

(* location -> int *)
let get_integer_from_loc l =
  match l with 
  Locat(t) -> 
    match t with 
    Obj(i) -> i
  |LNull -> failwith "the location is not assigned"
  |_ -> failwith "not a location"


(* when new frame is pushed on stack (decl, proc) allocate new empty heap entry (which is a list)*)  
(* when new field is declared the field is appended to the entry at l of the heap  *)
(* string -> ast -> tva -> int -> heap -> heap  *)
(** varible heap allocation is always the first of the heap, then fields allocation are appended *)
let heap_var_allocation s t v l hp = 
  if l < List.length hp then (Ident(s, Ptr(t)), ref v) ::(List.nth hp l) (**bug this is a heapentry but we want a heap*)
  else hp @ [Entry []::[(Ident(s, Ptr(t)), ref v)]]
;;

(* string -> tva -> int -> heap -> heap  *)
let heap_field_allocation s v l hp = 
  if l < List.length hp then (List.nth hp l) @ [(Field(s), ref v)] 
  else hp @ [Entry []::[(Field(s), ref v)]]
;;




(* given the location int l obtained from stack return the val that's in heap at this location that associated to the given ast ast_s*)
(* ast_s can be Field or Ident *)
(* ast -> int -> heap -> tva *) 
let get_val_from_heap ast_s l hp = 
  if l >= List.length hp then failwith "location l exceeded the size of heap"
  else let hpentrylist = List.nth hp l in
      if List.mem_assoc ast_s hpentrylist then !(List.assoc ast_s hpentrylist)
      else TvaNull(Null)
;;

(* use the index l to get the heaplist in old heap hp, find the Ident or Field *)
(* if it's Ident then pop head of heaplist *)
(* else if Field then remove the assoc of the heaplist, appeand the new heapentry *)
(* return this new list *)
(* let l2 = ("b", 1) :: List.remove_assoc "b" l *)
(* ast -> int -> heap -> heap *) 
let change_val_in_heap ast_s l tva hp = 
  if l >= List.length hp then failwith "location l exceeded the size of heap"
  else insert ast_s l tva hp
;;

let rec insert ast_s l tva hp = 
  match hp with
    [] -> failwith "the ast_s to be set value with is not in hp"
    |h::t ->  if l = 0 then
                if List.mem_assoc ast_s h then 
                  match ast_s with 
                    Ident(s, p) -> Entry((ast_s, ref tva):: List.remove_assoc ast_s h)::t
                    |_ -> List.remove_assoc ast_s h @ ([(ast_s, ref tva)]:heapentry)::t
                else failwith "no such variable/field on the heap at location l"
              else let hp' = insert ast_s l-1 tva t in 
                h::hp'
;;

(* ast -> stack -> hp -> tva *)
let rec eval ast_s st hp = 
  match ast_s with 
  Num(i) -> TvaInt(i)
  |Field(s) -> TvaField(ast_s)
  |Ident(s, t) -> match t with 
                    |Ptr(ast_p) ->  let loc = get_loc_from_stack ast_s st in 
                                    let l = get_integer_from_loc loc in
                                    let v = get_val_from_heap ast_s l hp in
                                      match v with
                                        TvaNull(Null) -> failwith "variable is not declared"
                                        |_ -> v
                    |_ -> failwtih "variable not decl-linked"
  |Diff(e1, e2) -> match e1, e2 with 
                    Num(i1), Num(i2) -> TvaInt(i1-i2)
                    |_, _ -> print_string "e1-e2 eval failure"; TvaError
  |Null -> print_string "ast_s is Null"; TvaNull(Null)
  
  |Loc(e1, e2) -> let le1 = eval e1 st hp in 
                  let fe2 = eval e2 st hp in
                    match le1, fe2 with 
                      TvaLoc(loc), TvaField(ast_f) ->  let l = get_integer_from_loc loc in 
                                                        let val = get_val_from_heap ast_f l hp
                                                          match val with 
                                                            TvaNull(Null) -> (** push the declared field into the heap entry and assign it*)
                                                            |_ -> val
                                                          
                      |_, _ -> print_string "e1.e2 eval failure"; TvaError
                  
  |Proc(s, c) -> TvaClo(Clo(Ident(s, Ptr(ast_s)), c, st))
  |_ -> TvaError
;;

(* ast -> stack -> heap -> boolean *)
let rec bool_eval ast_s st hp =
  match ast_s with
  True -> BoolT
  |False -> BoolF
  |Equal(e1, e2) -> let v1 = eval e1 st hp in
                    let v2 = eval e2 st hp in
                      match v1, v2 with 
                        TvaInt(i1), TvaInt(i2) -> if i1 = i2 then BoolT else BoolF
                        |TvaLoc(l1), TvaLoc(l2) -> match l1, l2 with 
                                                    Locat(obj1), Locat(obj2) -> match obj1, obj2 with 
                                                                                  Obj(i1), Obj(i2) -> if i1 = i2 then BoolT else BoolF
                        |TvaClo(clo1), TvaClo(clo2) -> if clo1 = clo2 then BoolT else BoolF
                        |_, _ -> BoolErr
  |LessT(e1, e2) -> let v1 = eval e1 st hp in
                    let v2 = eval e2 st hp in
                      match v1, v2 with 
                        TvaInt(i1), TvaInt(i2) -> if i1 < i2 then BoolT else BoolF
                        |_, _ -> BoolErr
  |_ -> BoolErr

    
let rec ptr_cmd t st hp= 
  match t with 
    Empty -> Conf (Empty, st, hp)  (* I need to stop execution here? How? And do I need to poop st here? *)(** at the end of the execution there is an empty stack*)

    |Proc (s, c) ->  let l = List.length hp in
                      Conf(Block(c), (push_stack s t l st), (heap_var_allocation s t TvaNull l hp)); 
                  
    |Decl (s, c) -> let l = List.length hp in 
                      Conf(Block(c), (push_stack s t l st), (heap_var_allocation s t TvaNull l hp)); 
                  
    |Seq (c1, c2) -> let Conf(c1', st', hp') = ptr_cmd c1 st hp in
                      match c1' with 
                        Empty -> Conf(c2, st', hp')
                        |_ -> Conf(Seq(c1', c2), st', hp')
    
    |Block (c) -> match c with 
                    Empty -> Conf (Empty, pop st, hp) 
                    |_ -> Conf(c, st, hp)
                       
    |Assign(s, p, e) -> let v = eval e st hp in 
                          match v with 
                            TvaErr -> failwith "assigning invalid expression"
                            |_ -> let ast_s = Ident(s, p) in
                                  let loc = get_loc_from_stack ast_s st in 
                                  let l = get_integer_from_loc loc in
                                    Conf(Empty, st, change_val_in_heap ast_s l v hp) (*retrun the Empty, st, hp(modified)*)

    |FieldAssign(e1, e2, e3) -> let ast_s = Loc(e1, e2) in 
                                let v = eval ast_s st hp in
                                   match v with 
                                    TvaErr -> failwith "Loc(e1, e2) not in current heap "
                                    |_ -> let v1 = eval e1 st hp in
                                          let v2 = eval e2 st hp in
                                          let v' = eval e3 st hp in 
                                            match v1, v2 with 
                                              TvaLoc(loc), TvaField(ast_s') -> let l = get_integer_from_loc loc in 
                                                                                Conf(Empty, st, change_val_in_heap ast_s' l v' hp)
                                              |TvaLoc(loc), _ -> let l = get_integer_from_loc loc in
                                                                  (** push the declared field into the heap entry and assign it*)
                                              |_ -> failwith "in fieldassign shouldn't ever come here but just incase cause eval should already catch this "
    (**i kind of decided that malloc can only be called on ident*)
    (* this is for sure bugged, I need to set the field that malloced to be null or find out how the field will be evaluated in e1.e2 sort of setting *)
    (* currently e1.e2 assumes e2 evaluated to TvaField, meaning e2 has already existed in the heap *)
    (* so when does e2 get to be initiated on the heap? *)
    |Malloc(ast_s) -> let newl = List.length hp in
                      let loc = get_loc_from_stack ast_s st in
                      let l = get_integer_from_loc loc in
                        Conf (Empty, st, change_val_in_heap ast_s l TvaLoc(Locat(Obj(newl))) hp)


    |RecProcCall(e1, e2) -> let v = eval e1 st hp in 
                              match v with 
                                TvaClo(clo1) -> match clo1 with 
                                                  Clo(ast_s, c, st') -> let l = List.length hp in
                                                                        let recv = eval e2 st hp in
                                                                          Conf(Block c, push_stack ast_s l st', change_val_in_heap ast_s l recv hp)                          
                                                  |_ -> failwith "recursive call closure not valid"
                                |_ -> failwith "recursive call on not a closure"
  
    |If(condition, c1, c2) -> let b = bool_eval(condition, st, hp) in
                                match b with
                                  BoolT -> Conf(c1, st, hp)
                                  |BoolF -> Conf(c2, st, hp)
                                  |_ -> failwith "if condition evaluation error"

    |While(condition, c) -> let b = bool_eval(condition, st, hp) in
                                match b with 
                                  BoolT -> Conf(c, st, hp)
                                  |BoolF -> Conf(Empty, st, hp)
                                  |_ -> failwith "if condition evaluation error"
    
    |Atom(c) -> let Conf(t', st', hp') = ptr_cmd c st hp in 
                  match t' with (* fishy recurssion *)
                    Empty -> Conf(Empty, st', hp')
                    |_ -> ptr_cmd Atom(t') st' hp'

    |Parallel(c1, c2) -> let i = Random.int 2 in 
                          if i = 0 then let Conf(t', st', hp') = ptr_cmd c1 st hp in 
                            match t' with
                              |Empty -> Conf(c2, st', hp')
                              |_ -> Conf(Parallel(t', c2), st', hp')
                          else let Conf(t', st', hp') = ptr_cmd c2 st hp in 
                            match t' with
                              |Empty -> Conf(c1, st', hp')
                              |_ ->Conf(Parallel(c1, t'), st', hp')

    |Skip -> Conf(Empty, st, hp)

    |_ -> st;;
  
(* the type was incompatible thus invented the ast type to unite the cmdAST and exprAST type *)
(* should I initiate heap here so the function is a trace function *)
(* ast -> int -> stack -> heap -> configuration *)
let rec trace t st hp = 
  match t with 
    Empty -> failwith "empty is not a program"
    |_ -> let Conf(t', st', hp') = ptr_cmd t st hp in
            match t' with 
            |Empty -> Conf(Empty, st', hp')
            |_ -> trace t' st' hp'
;;

(* let rec print_exprast expr = 
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
    | _  -> failwith "print_ast";; *)

