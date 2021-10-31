
module MenhirBasics = struct
  
  exception Error
  
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
    | NUM of (
# 15 "miniooMENHIR.mly"
       ( int )
# 20 "miniooMENHIR.ml"
  )
    | NULL
    | MINUS
    | MALLOC
    | LT
    | LPAREN
    | LOCATION
    | LCUR
    | IF
    | IDENT of (
# 13 "miniooMENHIR.mly"
       ( string )
# 33 "miniooMENHIR.ml"
  )
    | FIELD of (
# 14 "miniooMENHIR.mly"
       ( string )
# 38 "miniooMENHIR.ml"
  )
    | FALSE
    | EQUALS
    | EOF
    | ELSE
    | COLON
    | ATOM
    | ASSIGN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState47
  | MenhirState42
  | MenhirState38
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState15
  | MenhirState14
  | MenhirState8
  | MenhirState5
  | MenhirState1
  | MenhirState0

# 1 "miniooMENHIR.mly"
   (* header *)
  
open MiniooAbstractSyntax


# 85 "miniooMENHIR.ml"

let rec _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 90 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FIELD _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | NULL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NUM _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 114 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FIELD _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | NULL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | NUM _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_bool : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 143 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 153 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LCUR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MALLOC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SKIP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | VAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv192)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 181 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | IF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LCUR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MALLOC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SKIP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | VAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv194)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 210 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 220 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 230 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FIELD _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | IDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | NULL ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NUM _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv148)
        | LOCATION ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 256 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FIELD _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | IDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | NULL ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | NUM _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv150)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 284 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv159 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 293 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 297 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOF | EQUALS | IDENT _ | IF | LCUR | LT | MALLOC | MINUS | RCUR | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv155 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 309 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 313 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 318 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 322 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 327 "miniooMENHIR.ml"
            ) = 
# 54 "miniooMENHIR.mly"
                                  ( Diff(e1, e2) )
# 331 "miniooMENHIR.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv157 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 341 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 345 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 354 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 358 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOF | EQUALS | IDENT _ | IF | LCUR | LT | MALLOC | RCUR | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv161 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 372 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 376 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 381 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 385 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 390 "miniooMENHIR.ml"
            ) = 
# 58 "miniooMENHIR.mly"
                                  ( Loc(e1, e2) )
# 394 "miniooMENHIR.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv163 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 404 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 408 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 417 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 421 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | IDENT _ | IF | LCUR | MALLOC | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv167 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 435 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 439 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 444 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 448 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 453 "miniooMENHIR.ml"
            ) = 
# 66 "miniooMENHIR.mly"
                                  ( LessT(e1, e2) )
# 457 "miniooMENHIR.ml"
             in
            _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 467 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 471 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv177 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 480 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 484 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | IDENT _ | IF | LCUR | MALLOC | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv173 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 498 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 502 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 507 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 511 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 516 "miniooMENHIR.ml"
            ) = 
# 65 "miniooMENHIR.mly"
                                  ( Equal(e1, e2) )
# 520 "miniooMENHIR.ml"
             in
            _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv175 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 530 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 534 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv189 * _menhir_state * (
# 13 "miniooMENHIR.mly"
       ( string )
# 543 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 547 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | EOF | EQUALS | IDENT _ | IF | LCUR | LT | MALLOC | RCUR | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv185 * _menhir_state * (
# 13 "miniooMENHIR.mly"
       ( string )
# 561 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 565 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (x : (
# 13 "miniooMENHIR.mly"
       ( string )
# 570 "miniooMENHIR.ml"
            ))), _, (e : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 574 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 18 "miniooMENHIR.mly"
      ( cmdAST )
# 579 "miniooMENHIR.ml"
            ) = 
# 38 "miniooMENHIR.mly"
                               ( Assign(x, e) )
# 583 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 18 "miniooMENHIR.mly"
      ( cmdAST )
# 591 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv181) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 18 "miniooMENHIR.mly"
      ( cmdAST )
# 599 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 18 "miniooMENHIR.mly"
      ( cmdAST )
# 607 "miniooMENHIR.ml"
            )) : (
# 18 "miniooMENHIR.mly"
      ( cmdAST )
# 611 "miniooMENHIR.ml"
            )) = _v in
            ((let _v : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 616 "miniooMENHIR.ml"
            ) = 
# 32 "miniooMENHIR.mly"
                ( _1 )
# 620 "miniooMENHIR.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)) : 'freshtv186)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv187 * _menhir_state * (
# 13 "miniooMENHIR.mly"
       ( string )
# 630 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 634 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)
    | _ ->
        _menhir_fail ()

and _menhir_goto_seqctrl : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 644 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 653 "miniooMENHIR.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 661 "miniooMENHIR.ml"
    )) : (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 665 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 670 "miniooMENHIR.ml"
    ) = 
# 34 "miniooMENHIR.mly"
                ( _1 )
# 674 "miniooMENHIR.ml"
     in
    _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv144)) : 'freshtv146)

and _menhir_goto_cmd : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 681 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 691 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 695 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv87 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 705 "miniooMENHIR.ml"
            )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 709 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LCUR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | MALLOC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv88)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv89 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 739 "miniooMENHIR.ml"
            )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 743 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)) : 'freshtv92)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv95 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 752 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 756 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 760 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv93 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 766 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 770 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 774 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 779 "miniooMENHIR.ml"
        ))), _, (_3 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 783 "miniooMENHIR.ml"
        ))), _, (_5 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 787 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 792 "miniooMENHIR.ml"
        ) = 
# 50 "miniooMENHIR.mly"
                                  ( If(_2, _3, _5) )
# 796 "miniooMENHIR.ml"
         in
        _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)) : 'freshtv96)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 804 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv97 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 814 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | LCUR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | MALLOC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv98)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 844 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)) : 'freshtv102)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv109 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 853 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 857 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RCUR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv105 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 867 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 871 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv103 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 878 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 882 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 887 "miniooMENHIR.ml"
            ))), _, (_4 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 891 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 896 "miniooMENHIR.ml"
            ) = 
# 48 "miniooMENHIR.mly"
                                  ( Seq(_2, _4) )
# 900 "miniooMENHIR.ml"
             in
            _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv104)) : 'freshtv106)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv107 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 910 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 914 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)) : 'freshtv110)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv119 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 923 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 927 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv117 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 933 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 937 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (x : (
# 13 "miniooMENHIR.mly"
       ( string )
# 942 "miniooMENHIR.ml"
        ))), _, (_4 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 946 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 19 "miniooMENHIR.mly"
      ( cmdAST )
# 951 "miniooMENHIR.ml"
        ) = 
# 41 "miniooMENHIR.mly"
                                ( Decl(x, _4) )
# 955 "miniooMENHIR.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 19 "miniooMENHIR.mly"
      ( cmdAST )
# 963 "miniooMENHIR.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 19 "miniooMENHIR.mly"
      ( cmdAST )
# 971 "miniooMENHIR.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 19 "miniooMENHIR.mly"
      ( cmdAST )
# 979 "miniooMENHIR.ml"
        )) : (
# 19 "miniooMENHIR.mly"
      ( cmdAST )
# 983 "miniooMENHIR.ml"
        )) = _v in
        ((let _v : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 988 "miniooMENHIR.ml"
        ) = 
# 33 "miniooMENHIR.mly"
                ( _1 )
# 992 "miniooMENHIR.ml"
         in
        _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv112)) : 'freshtv114)) : 'freshtv116)) : 'freshtv118)) : 'freshtv120)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv123 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1000 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1004 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv121 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1010 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1014 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1019 "miniooMENHIR.ml"
        ))), _, (_4 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1023 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1028 "miniooMENHIR.ml"
        ) = 
# 59 "miniooMENHIR.mly"
                                  ( Proc(_2, _4) )
# 1032 "miniooMENHIR.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)) : 'freshtv124)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv127 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1040 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1044 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1050 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1054 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1059 "miniooMENHIR.ml"
        ))), _, (_3 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1063 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 1068 "miniooMENHIR.ml"
        ) = 
# 49 "miniooMENHIR.mly"
                                  ( While(_2, _3) )
# 1072 "miniooMENHIR.ml"
         in
        _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)) : 'freshtv128)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1080 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv137 * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1090 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv135 * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1096 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1101 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.cmdAST)
# 1106 "miniooMENHIR.ml"
            ) = 
# 29 "miniooMENHIR.mly"
            ( _1 )
# 1110 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv133) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.cmdAST)
# 1118 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.cmdAST)
# 1126 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.cmdAST)
# 1134 "miniooMENHIR.ml"
            )) : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.cmdAST)
# 1138 "miniooMENHIR.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv130)) : 'freshtv132)) : 'freshtv134)) : 'freshtv136)) : 'freshtv138)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139 * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1148 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)) : 'freshtv142)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1164 "miniooMENHIR.ml"
    ) = 
# 63 "miniooMENHIR.mly"
                                  ( True )
# 1168 "miniooMENHIR.ml"
     in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv86)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1181 "miniooMENHIR.ml"
    ) = 
# 64 "miniooMENHIR.mly"
                                  ( False )
# 1185 "miniooMENHIR.ml"
     in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv84)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1201 "miniooMENHIR.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv75 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1212 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | LCUR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | MALLOC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv76)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv77 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1242 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)) : 'freshtv80)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "miniooMENHIR.mly"
       ( int )
# 1257 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : (
# 15 "miniooMENHIR.mly"
       ( int )
# 1267 "miniooMENHIR.ml"
    )) : (
# 15 "miniooMENHIR.mly"
       ( int )
# 1271 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1276 "miniooMENHIR.ml"
    ) = 
# 55 "miniooMENHIR.mly"
                                  ( Num v )
# 1280 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv74)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv71) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1293 "miniooMENHIR.ml"
    ) = 
# 57 "miniooMENHIR.mly"
                                  ( Null )
# 1297 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv72)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "miniooMENHIR.mly"
       ( string )
# 1304 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1314 "miniooMENHIR.ml"
    )) : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1318 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1323 "miniooMENHIR.ml"
    ) = 
# 56 "miniooMENHIR.mly"
                                  ( Ident v )
# 1327 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv70)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "miniooMENHIR.mly"
       ( string )
# 1334 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv67) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((f : (
# 14 "miniooMENHIR.mly"
       ( string )
# 1344 "miniooMENHIR.ml"
    )) : (
# 14 "miniooMENHIR.mly"
       ( string )
# 1348 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1353 "miniooMENHIR.ml"
    ) = 
# 53 "miniooMENHIR.mly"
                                  ( Field(f) )
# 1357 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv68)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1369 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv39 * _menhir_state) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1378 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv41 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1387 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1391 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1400 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( boolEXPR )
# 1409 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1418 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1427 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1436 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( exprAST )
# 1445 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv59 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1464 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv61 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1473 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv66)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FIELD _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | NULL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NUM _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1524 "miniooMENHIR.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv29 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1535 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | LCUR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | MALLOC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv30)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv31 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1565 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)) : 'freshtv34)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 20 "miniooMENHIR.mly"
      ( cmdAST )
# 1586 "miniooMENHIR.ml"
    ) = 
# 47 "miniooMENHIR.mly"
                                  ( Skip )
# 1590 "miniooMENHIR.ml"
     in
    _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv19 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1612 "miniooMENHIR.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv15 * _menhir_state)) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1623 "miniooMENHIR.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv13 * _menhir_state)) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1630 "miniooMENHIR.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), (x : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1635 "miniooMENHIR.ml"
                ))) = _menhir_stack in
                let _v : 'tv_malloc = 
# 44 "miniooMENHIR.mly"
                                   ( Malloc(x) )
# 1640 "miniooMENHIR.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv11) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_malloc) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_malloc) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_malloc) : 'tv_malloc) = _v in
                ((let _v : (
# 21 "miniooMENHIR.mly"
      ( cmdAST )
# 1657 "miniooMENHIR.ml"
                ) = 
# 35 "miniooMENHIR.mly"
                ( _1 )
# 1661 "miniooMENHIR.ml"
                 in
                _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)) : 'freshtv10)) : 'freshtv12)) : 'freshtv14)) : 'freshtv16)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv17 * _menhir_state)) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1671 "miniooMENHIR.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)) : 'freshtv20)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv21 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LCUR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | MALLOC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | SKIP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | VAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | FIELD _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | NULL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NUM _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "miniooMENHIR.mly"
       ( string )
# 1743 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1755 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FIELD _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | IDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | NULL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NUM _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | PROCEDURE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1781 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.cmdAST)
# 1801 "miniooMENHIR.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LCUR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MALLOC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SKIP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 68 "miniooMENHIR.mly"
   (* trailer *)

# 1837 "miniooMENHIR.ml"

# 269 "<standard.mly>"
  

# 1842 "miniooMENHIR.ml"
