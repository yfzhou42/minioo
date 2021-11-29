(* File miniooAbstractSyntax.ml *)

type ast =
  Ident of string * ptr_decl (**check the delcaration*)
  | Field of string 
  | Num of int
  | Diff of ast * ast 
  | Null 
  | Loc of ast * ast
  
  | Proc of string * ast
  | Decl of string * ast
  
  | NewAssign of ast * ast
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
  | TvaNull 
;;
  
type heapentry =  
  Entry of (ast * tva ref) list 
and heap = heapentry list 
;;

type configuration = 
  Conf of ast * stack * heap
;;

(* ################## stack operations ####################*)
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
  |_ -> failwith "not a location"

(* ####################### end of stack operation ################################*)

(* ####################### variable linking/checking with declaration and proc ############################# *)
(* string -> ast -> ast *)
let link_var s t = 

     match t with 
        Decl(s, c) -> Ident(s, Ptr(t))
        |Proc(s, c) -> Ident(s, Ptr(t))
        |_ -> failwith "the link is not of type decl or proc"
   
;;

(* used at Assign/Malloc *)
(* ast -> unit() *)
let check_varlinking ast_v =
    match ast_v with 
      Ident(s, p) -> match p with 
                      Ptr(ast_d_or_p) -> match ast_d_or_p with 
                                          Decl(s', ast') -> print_string "variable is correctly linked to decl";
                                          |Proc(s', ast') -> print_string "variable is correctly linked to proc";
                                          |_ -> failwith "varable linked with ast other than decl/proc"
                      |_ -> failwith "variable not linked"
      |_ -> failwith "not a variable"

(* linking ast return a linked_ast 
(* ast -> ast *)
let rec link_ast ast = 
  match ast with 
    Empty -> failwith "ast has Empty, linking does not happen on empty"

    |Ident(s, p) -> check_varlinking ast; ast;
    
    |Decl(ast_i, c) -> match ast_i with
                        Ident(s, _) -> link_ast(Ident(s, Ptr(ast))); link_ast c;
    |Proc(ast_i, c) -> match ast_i with
                        Ident(s, _) -> link_ast(Ident(s, Ptr(ast))); link_ast c;

    |Seq (c1, c2) -> link_ast c1; link_ast c2;

    |NewAssign(e1, e2) -> match e1 with 
                            Ident() -> 
                            |Loc() ->
                            |_ ->
    |Malloc(ast_s) ->
    
    |RecProcCall(e1, e2) ->

    |If(condition, c1, c2) ->

    |While(condition, c) ->

    |Atom(c) ->

    |Parallel(c1, c2) ->

    |Skip ->

    |_ -> failwith "to catch impossible failure in linking"
;; *)
(* 

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
(* given the location int l obtained from stack, get the heapentry using l, return the val that's in this heapentry that associated to the given ast ast_s*)
(* ast_s can be Field or Ident *)
(* ast -> int -> heap -> tva *) 
let get_val_from_heap ast_s l hp = 
  if l >= List.length hp then failwith "location l exceeded the size of heap"
  else let hpentrylist = List.nth hp l in
      if List.mem_assoc ast_s hpentrylist then !(List.assoc ast_s hpentrylist)
      else TvaNull
;;

(* use the index l to get the heapentry in old heap hp at l, find the Ident or Field *)
(* if it's Ident then pop head of heaplist *)
(* else if Field then remove the assoc of the heapentry, appeand the new heapentry *)
(* return this new list *)
(* let l2 = ("b", 1) :: List.remove_assoc "b" l *)

(* This ast_s must exist in the heap already *)
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
;; *)

(* ################### refactoring heap ############################## *)

(* the bricks I need *)
(* heapentry -> (ast * tav ref) list *)
let get_list_from_heapentry hpent = 
  match hpent with
  Entry(ast_tvaref_list) -> ast_tvaref_list
  |_ -> failwith "it's not a heapentry"
;;

(* ast is Ident/Field, l is int *) 
(* ast -> int -> heap -> tva *)
let get_val_from_heap ast l hp = 
  if l >= List.length hp then failwith "location l exceeded the size of heap"
  else 
    let hpentrylist = get_list_from_heapentry (List.nth hp l) in
      if List.mem_assoc ast hpentrylist then !(List.assoc ast hpentrylist)
      else TvaNull;
  ;;

(* a recursive helper function for set/change_val_in_heap to insert the new (ast * tva ref) association pair in heap *)
(* ast -> int -> tva -> heap -> heap *)
let rec insert ast l v hp =
  if l >= List.length hp then failwith "location l exceeded the size of heap"
  else 
    match hp with  
      [] -> failwith "would fail here but still this is when we get a empty hp"
      |h::t -> if l = 0 then 
                let hpentrylist = get_list_from_heapentry h in  
                  Entry((ast, ref(v))::hpentrylist)::t
               else h::insert ast (l-1) v t
;;
(* this is when ast doesn't exist in heap yet *)
(* ast -> int -> tva -> heap -> heap *)
let rec set_val_in_heap ast l v hp =
  if l >= List.length hp then failwith "location l exceeded the size of heap"
  else 
    match hp with  
      [] -> failwith "would fail here but still this is when we get a empty hp"
      |h::t -> if l = 0 then 
                let hpentrylist = get_list_from_heapentry h in  
                  Entry((ast, ref(v))::hpentrylist)::t
               else h::set_val_in_heap ast (l-1) v t
;;

(* this is when ast already exist in heap and we want to change the value to cuurent v *)
(* first disassociate the old (ast, ref tva), then call insert *)
(* ast -> int -> tva -> heap -> heap *)
let rec change_val_in_heap ast l v hp =
  if l >= List.length hp then failwith "location l exceeded the size of heap"
  else 
    match hp with  
      [] -> failwith "would fail here but still this is when we get a empty hp"
      |h::t ->  if l = 0 then 
                  let hpentrylist = get_list_from_heapentry h in  
                    Entry((ast, ref(v))::List.remove_assoc ast hpentrylist)::t
                else h::change_val_in_heap ast (l-1) v t

;;


(* ################### heap ends ############################## *)

  (*1. since malloc should have happened in previous cmds, eval of ast_v should return location. Check this, if not fail
    2. given 1. did not error we eval ast_f. And this should give TvaField(Field). If not fail
    (after this point all happen in get_val_from_heap) 
    3. given 1.2. succeeded use loc got in 1. check in heap with this loc use get_val_from_heap ast_f l hp, (use List.mem_assoc hp[loc] Field). 
        the returned val of get_val_from_heap will either be 
        (TvaNull, meaning field has not existed yet, or TvaErr (unclear why it would be but we shall fail if so), or everything else doesn't matter)
    If exist update the value to v, 
      if doesn't exit, we need to push this Field into that hp[loc] and set value to TvaNull at this point 
    4. the provious steps have made sure the field exists. now call change_val_in_heap with  *)
(* ast -> stack -> hp -> tva *)
let rec eval ast_s st hp = 
  match ast_s with 
  Num(i) -> TvaInt(i)
  |Field(s) -> TvaField(ast_s)
  |Ident(s, t) -> check_varlinking ast_s; 
                  let loc = get_loc_from_stack ast_s st in 
                  let l = get_integer_from_loc loc in
                    get_val_from_heap ast_s l hp 
  
  
  |Loc(e1, e2) -> let le1 = eval e1 st hp in 
                    let fe2 = eval e2 st hp in
                    (match le1, fe2 with 
                      TvaLoc(loc), _ -> (match fe2 with 
                                            TvaField(ast_f) ->  let l = get_integer_from_loc loc in 
                                                                    get_val_from_heap ast_f l hp;
                                            |_ -> print_string "e2 in e1.e2 has to be field"; TvaError
                                        )                 
                      |_, _ -> print_string "there weren't malloc for variable before this"; TvaError; 
                    )
  |Diff(e1, e2) -> (match e1, e2 with 
                      Num(i1), Num(i2) -> TvaInt(i1-i2)
                      |_, _ -> failwith "e1-e2 eval failure"
                    )
  |Proc(s, c) -> TvaClo(Clo(Ident(s, Ptr(ast_s)), c, st))

  |Null -> print_string "ast_s at eval is Null"; TvaError

  |_ -> TvaError
;;

(* ast -> stack -> heap -> boolean *)
let rec bool_eval ast_s st hp =
  match ast_s with
  True -> BoolT
  |False -> BoolF
  |Equal(e1, e2) -> let v1 = eval e1 st hp in
                    let v2 = eval e2 st hp in
                      (match v1, v2 with 
                        TvaInt(i1), TvaInt(i2) -> if i1 = i2 then BoolT else BoolF
                        |TvaLoc(l1), TvaLoc(l2) -> (match l1, l2 with 
                                                    Locat(obj1), Locat(obj2) -> (match obj1, obj2 with 
                                                                                  Obj(i1), Obj(i2) -> if i1 = i2 then BoolT else BoolF
                                                                                )
                                                    )
                        |TvaClo(clo1), TvaClo(clo2) -> if clo1 = clo2 then BoolT else BoolF
                        |_, _ -> BoolErr
                      )
  |LessT(e1, e2) -> let v1 = eval e1 st hp in
                    let v2 = eval e2 st hp in
                      (match v1, v2 with 
                        TvaInt(i1), TvaInt(i2) -> if i1 < i2 then BoolT else BoolF
                        |_, _ -> BoolErr
                      )
  |_ -> BoolErr
;;
    

let rec ptr_cmd t st hp= 
  match t with 
    Empty -> Conf (Empty, st, hp)  (* I need to stop execution here? How? And do I need to poop st here? *)(** at the end of the execution there is an empty stack*)

    |Proc (s, c) ->  let l = List.length hp in
                     let ast_s = link_var s t in 
                      Conf(Block(c), (push_stack ast_s l st), (set_val_in_heap ast_s l TvaNull hp)); 
                  
    |Decl (s, c) -> let l = List.length hp in 
                    let ast_s = link_var s t in 
                      Conf(Block(c), (push_stack ast_s l st), (set_val_in_heap ast_s l TvaNull hp)); 
                  
    |Seq (c1, c2) -> let Conf(c1', st', hp') = ptr_cmd c1 st hp in
                      (match c1' with 
                        Empty -> Conf(c2, st', hp')
                        |_ -> Conf(Seq(c1', c2), st', hp')
                      )
    |Block (c) -> (match c with 
                    Empty -> Conf (Empty, pop_stack st, hp) 
                    |_ -> Conf(c, st, hp)
                  )     
    (* we need to be clear only variable can have field, thus in Loc(e1, e2) e1 can only be Ident and e2 be field *)
    
    |NewAssign(e1, e2) -> let v = eval e2 st hp in  (* eval of e2 shuold have and return a Tva value , check this if not error out *)
                            (match e1 with 
                              Ident(s, p) -> check_varlinking e1; 
                                            let loc = get_loc_from_stack e1 st in 
                                            let l = get_integer_from_loc loc in
                                              Conf(Empty, st, change_val_in_heap e1 l v hp) (*retrun the Empty, st, hp(modified)*)

                              (* we call eval on Loc
                              in eval of Loc(ast_v, ast_f) are restriced to: ast_v can only be Ident and ast_f be Field that's not true e2 can be other Tva val 
                              and in eval of Loc a few things should happen (see commet at eval for details):*)
                              
                              |Loc(ast_v, ast_f) -> let val_loc = eval e1 st hp in 
                                                      (match val_loc with 
                                                        TvaError -> failwith "eval of e1.e2 fail"
                                                        |TvaNull -> let tvaloc = eval ast_v st hp in
                                                                    (match tvaloc with 
                                                                      TvaLoc(loc) -> let l = get_integer_from_loc loc in 
                                                                                      Conf(Empty, st, set_val_in_heap ast_f l v hp) (* we now create the field at the location and set that value to v*)
                                                                      |_ -> failwith "not a location in e1.e2"
                                                                    )
                                                        (** if that field already exists in heap then just change the field *)
                                                        |_ -> let tvaloc = eval ast_v st hp in
                                                                (match tvaloc with 
                                                                  TvaLoc(loc) -> let l = get_integer_from_loc loc in 
                                                                                  Conf(Empty, st, change_val_in_heap ast_f l v hp)(* we now create the field at the location and set that value to v*)
                                                                  |_ -> failwith "not a location in e1.e2"
                                                                )
                                                      )
                              |_ -> failwith "assign only takes Ident or Loc, a third type is caught here"
                            )
    (* |Assign(s, p, e) -> let v = eval e st hp in 
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
    *i kind of decided that malloc can only be called on ident *)
    (* this is for sure bugged, I need to set the field that malloced to be null or find out how the field will be evaluated in e1.e2 sort of setting *)
    (* currently e1.e2 assumes e2 evaluated to TvaField, meaning e2 has already existed in the heap *)
    (* so when does e2 get to be initiated on the heap? *)
    |Malloc(ast_s) -> check_varlinking ast_s; 
                      let newl = List.length hp in 
                      let newloc = TvaLoc(Locat(Obj(newl))) in
                      let loc = get_loc_from_stack ast_s st in
                      let l = get_integer_from_loc loc in

                      let hp' = change_val_in_heap ast_s l newloc hp in
                        Conf (Empty, st, hp')


    |RecProcCall(e1, e2) -> let v = eval e1 st hp in 
                              (match v with 
                                TvaClo(clo1) -> (match clo1 with 
                                                  Clo(ast_s, c, st') -> let l = List.length hp in
                                                                        let recv = eval e2 st hp in
                                                                          (match recv with 
                                                                          TvaError -> failwith "recursive call e2 errored at eval"
                                                                          |_ -> Conf(Block c, push_stack ast_s l st', set_val_in_heap ast_s l recv hp) 
                                                                          )                         
                                                  |_ -> failwith "recursive call closure not valid"
                                                )
                                |_ -> failwith "recursive call on not a closure"
                              )
    
    
    |Atom(c) -> let Conf(t', st', hp') = ptr_cmd c st hp in 
                  (match t' with (* fishy recurssion *)
                    Empty -> Conf(Empty, st', hp')
                    |_ -> ptr_cmd (Atom(t')) st' hp'
                  )
    |Parallel(c1, c2) -> let i = Random.int 2 in 
                          if i = 0 then let Conf(t', st', hp') = ptr_cmd c1 st hp in 
                            (match t' with
                              |Empty -> Conf(c2, st', hp')
                              |_ -> Conf(Parallel(t', c2), st', hp')
                            )
                          else let Conf(t', st', hp') = ptr_cmd c2 st hp in 
                            (match t' with
                              |Empty -> Conf(c1, st', hp')
                              |_ ->Conf(Parallel(c1, t'), st', hp')
                            )

    |Skip -> Conf(Empty, st, hp)

    |If(condition, c1, c2) -> let b = bool_eval condition st hp in
                                (match b with
                                  BoolT -> Conf(c1, st, hp)
                                  |BoolF -> Conf(c2, st, hp)
                                  |_ -> failwith "If condition evaluation error"
                                )
    |While(condition, c) -> let b = bool_eval condition st hp in
                                (match b with 
                                  BoolT -> Conf(c, st, hp)
                                  |BoolF -> Conf(Empty, st, hp)
                                  |_ -> failwith "While condition evaluation error"
                                )
    |_ -> failwith "just to catch impossible failure in ptr_cmd";;
  
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

let rec print_ast ast = 
  match ast with 

    Num (i) -> print_string (string_of_int i) ;
    |Decl (s, cmdast) -> print_string ("Declaration:"^s) ;print_newline();
                         print_string ("Command List:") ;print_newline();
                         print_ast cmdast; print_newline()
    |NewAssign (ast_s, exprast) -> 
                            match ast_s with 
                              Ident(s, _) ->
                                print_string ("Assign: "^s) ;print_newline();
                                print_string (" = ") ;print_newline();
                                print_ast exprast; print_newline()
    | _  -> failwith "print_ast"
    
;;

