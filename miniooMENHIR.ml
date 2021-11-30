
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
  | MenhirState60
  | MenhirState55
  | MenhirState52
  | MenhirState49
  | MenhirState41
  | MenhirState38
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState13
  | MenhirState8
  | MenhirState5
  | MenhirState1
  | MenhirState0

# 1 "miniooMENHIR.mly"
   (* header *)
  
open MiniooAbstractSyntax


# 89 "miniooMENHIR.ml"

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_bool : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "miniooMENHIR.mly"
      ( ast )
# 99 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv249 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 109 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ATOM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | FIELD _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LCUR ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MALLOC ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NUM _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | PROCEDURE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SKIP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | VAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv250)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 147 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ATOM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | FIELD _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | IF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LCUR ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MALLOC ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NUM _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | PROCEDURE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SKIP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | VAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv252)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cmd : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "miniooMENHIR.mly"
      ( ast )
# 186 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv175 * _menhir_state)) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 196 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv171 * _menhir_state)) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 206 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state)) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 213 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (c : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 218 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : 'tv_atom = 
# 63 "miniooMENHIR.mly"
                                  ( Atom(c) )
# 223 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv167) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_atom) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv165) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_atom) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_atom) : 'tv_atom) = _v in
            ((let _v : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 240 "miniooMENHIR.ml"
            ) = 
# 38 "miniooMENHIR.mly"
                ( _1 )
# 244 "miniooMENHIR.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv164)) : 'freshtv166)) : 'freshtv168)) : 'freshtv170)) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv173 * _menhir_state)) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 254 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 263 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 267 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv177 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 277 "miniooMENHIR.ml"
            )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 281 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | LCUR ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | MALLOC ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 321 "miniooMENHIR.ml"
            )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 325 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv185 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 334 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 338 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 342 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv183 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 348 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 352 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 356 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : (
# 23 "miniooMENHIR.mly"
      ( ast )
# 361 "miniooMENHIR.ml"
        ))), _, (_3 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 365 "miniooMENHIR.ml"
        ))), _, (_5 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 369 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 21 "miniooMENHIR.mly"
      ( ast )
# 374 "miniooMENHIR.ml"
        ) = 
# 54 "miniooMENHIR.mly"
                                  ( If(_2, _3, _5) )
# 378 "miniooMENHIR.ml"
         in
        _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 386 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARALLEL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv187 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 396 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LCUR ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | MALLOC ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv188)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv189 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 434 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LCUR ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | MALLOC ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv191 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 474 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv201 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 483 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 487 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RCUR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv197 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 497 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 501 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv195 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 508 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 512 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 517 "miniooMENHIR.ml"
            ))), _, (_4 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 521 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 21 "miniooMENHIR.mly"
      ( ast )
# 526 "miniooMENHIR.ml"
            ) = 
# 52 "miniooMENHIR.mly"
                                  ( Seq(_2, _4) )
# 530 "miniooMENHIR.ml"
             in
            _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv199 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 540 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 544 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv215 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 553 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 557 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RCUR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv211 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 567 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 571 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv209 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 578 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 582 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (c1 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 587 "miniooMENHIR.ml"
            ))), _, (c2 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 591 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : 'tv_parallel = 
# 66 "miniooMENHIR.mly"
                                         ( Parallel(c1, c2) )
# 596 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_parallel) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_parallel) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_parallel) : 'tv_parallel) = _v in
            ((let _v : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 613 "miniooMENHIR.ml"
            ) = 
# 39 "miniooMENHIR.mly"
                ( _1 )
# 617 "miniooMENHIR.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv213 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 627 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 631 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv225 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 640 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 644 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv223 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 650 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 654 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (x : (
# 13 "miniooMENHIR.mly"
       ( string )
# 659 "miniooMENHIR.ml"
        ))), _, (_4 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 663 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 20 "miniooMENHIR.mly"
      ( ast )
# 668 "miniooMENHIR.ml"
        ) = 
# 44 "miniooMENHIR.mly"
                                ( Decl(x, _4) )
# 672 "miniooMENHIR.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "miniooMENHIR.mly"
      ( ast )
# 680 "miniooMENHIR.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 20 "miniooMENHIR.mly"
      ( ast )
# 688 "miniooMENHIR.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 20 "miniooMENHIR.mly"
      ( ast )
# 696 "miniooMENHIR.ml"
        )) : (
# 20 "miniooMENHIR.mly"
      ( ast )
# 700 "miniooMENHIR.ml"
        )) = _v in
        ((let _v : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 705 "miniooMENHIR.ml"
        ) = 
# 33 "miniooMENHIR.mly"
                ( _1 )
# 709 "miniooMENHIR.ml"
         in
        _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv229 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 717 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 721 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv227 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 727 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 731 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (y : (
# 13 "miniooMENHIR.mly"
       ( string )
# 736 "miniooMENHIR.ml"
        ))), _, (_4 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 740 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 745 "miniooMENHIR.ml"
        ) = 
# 76 "miniooMENHIR.mly"
                                  ( Proc(y, _4) )
# 749 "miniooMENHIR.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)) : 'freshtv230)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 757 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 761 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 767 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 771 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (_2 : (
# 23 "miniooMENHIR.mly"
      ( ast )
# 776 "miniooMENHIR.ml"
        ))), _, (_3 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 780 "miniooMENHIR.ml"
        ))) = _menhir_stack in
        let _v : (
# 21 "miniooMENHIR.mly"
      ( ast )
# 785 "miniooMENHIR.ml"
        ) = 
# 53 "miniooMENHIR.mly"
                                  ( While(_2, _3) )
# 789 "miniooMENHIR.ml"
         in
        _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 797 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 807 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 813 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 818 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.ast)
# 823 "miniooMENHIR.ml"
            ) = 
# 30 "miniooMENHIR.mly"
            ( _1 )
# 827 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.ast)
# 835 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv237) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.ast)
# 843 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.ast)
# 851 "miniooMENHIR.ml"
            )) : (
# 17 "miniooMENHIR.mly"
       (MiniooAbstractSyntax.ast)
# 855 "miniooMENHIR.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv236)) : 'freshtv238)) : 'freshtv240)) : 'freshtv242)) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 865 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 875 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FIELD _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NUM _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 899 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FIELD _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NUM _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_goto_seqctrl : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "miniooMENHIR.mly"
      ( ast )
# 923 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 21 "miniooMENHIR.mly"
      ( ast )
# 932 "miniooMENHIR.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 21 "miniooMENHIR.mly"
      ( ast )
# 940 "miniooMENHIR.ml"
    )) : (
# 21 "miniooMENHIR.mly"
      ( ast )
# 944 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 949 "miniooMENHIR.ml"
    ) = 
# 35 "miniooMENHIR.mly"
                ( _1 )
# 953 "miniooMENHIR.ml"
     in
    _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv160)) : 'freshtv162)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 23 "miniooMENHIR.mly"
      ( ast )
# 966 "miniooMENHIR.ml"
    ) = 
# 81 "miniooMENHIR.mly"
                                  ( True )
# 970 "miniooMENHIR.ml"
     in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 23 "miniooMENHIR.mly"
      ( ast )
# 983 "miniooMENHIR.ml"
    ) = 
# 82 "miniooMENHIR.mly"
                                  ( False )
# 987 "miniooMENHIR.ml"
     in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 24 "miniooMENHIR.mly"
      ( ast )
# 994 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state)) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1004 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv83 * _menhir_state)) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1018 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv81 * _menhir_state)) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1025 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (x : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1030 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : 'tv_malloc = 
# 57 "miniooMENHIR.mly"
                                  ( Malloc(x) )
# 1035 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv79) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_malloc) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv77) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_malloc) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_malloc) : 'tv_malloc) = _v in
            ((let _v : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 1052 "miniooMENHIR.ml"
            ) = 
# 36 "miniooMENHIR.mly"
                ( _1 )
# 1056 "miniooMENHIR.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv76)) : 'freshtv78)) : 'freshtv80)) : 'freshtv82)) : 'freshtv84)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv85 * _menhir_state)) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1066 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1075 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1079 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | ATOM | ELSE | EOF | EQUALS | FIELD _ | IDENT _ | IF | LCUR | LPAREN | LT | MALLOC | MINUS | NULL | NUM _ | PARALLEL | PROCEDURE | RCUR | RPAREN | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv89 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1091 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1095 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1100 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1104 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1109 "miniooMENHIR.ml"
            ) = 
# 72 "miniooMENHIR.mly"
                                  ( Diff(e1, e2) )
# 1113 "miniooMENHIR.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv91 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1123 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1127 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)) : 'freshtv94)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv99 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1136 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1140 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | ATOM | ELSE | EOF | EQUALS | FIELD _ | IDENT _ | IF | LCUR | LPAREN | LT | MALLOC | NULL | NUM _ | PARALLEL | PROCEDURE | RCUR | RPAREN | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv95 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1154 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1158 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1163 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1167 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1172 "miniooMENHIR.ml"
            ) = 
# 75 "miniooMENHIR.mly"
                                  ( Loc(e1, e2) )
# 1176 "miniooMENHIR.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv96)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv97 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1186 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1190 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
    | MenhirState1 | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1199 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv101 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1209 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv102)
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv103 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1235 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv104)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv105 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1263 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1272 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1276 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | ATOM | FIELD _ | IDENT _ | IF | LCUR | MALLOC | NULL | NUM _ | PROCEDURE | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv109 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1290 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1294 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1299 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1303 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 23 "miniooMENHIR.mly"
      ( ast )
# 1308 "miniooMENHIR.ml"
            ) = 
# 84 "miniooMENHIR.mly"
                                  ( LessT(e1, e2) )
# 1312 "miniooMENHIR.ml"
             in
            _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv111 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1322 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1326 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)) : 'freshtv114)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv119 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1335 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1339 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | ATOM | FIELD _ | IDENT _ | IF | LCUR | MALLOC | NULL | NUM _ | PROCEDURE | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv115 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1353 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1357 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1362 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1366 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 23 "miniooMENHIR.mly"
      ( ast )
# 1371 "miniooMENHIR.ml"
            ) = 
# 83 "miniooMENHIR.mly"
                                  ( Equal(e1, e2) )
# 1375 "miniooMENHIR.ml"
             in
            _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv117 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1385 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1389 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
    | MenhirState0 | MenhirState60 | MenhirState5 | MenhirState8 | MenhirState55 | MenhirState52 | MenhirState22 | MenhirState49 | MenhirState30 | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1398 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv121 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1408 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv122)
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv123 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1434 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv124)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv125 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1462 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)) : 'freshtv128)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv141 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1471 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1475 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv137 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1489 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1493 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv135 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1500 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1504 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1509 "miniooMENHIR.ml"
            ))), _, (e2 : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1513 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : 'tv_reccall = 
# 60 "miniooMENHIR.mly"
                                      ( RecProcCall(e1, e2) )
# 1518 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv133) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_reccall) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_reccall) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_reccall) : 'tv_reccall) = _v in
            ((let _v : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 1535 "miniooMENHIR.ml"
            ) = 
# 37 "miniooMENHIR.mly"
                ( _1 )
# 1539 "miniooMENHIR.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv130)) : 'freshtv132)) : 'freshtv134)) : 'freshtv136)) : 'freshtv138)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv139 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1549 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1553 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)) : 'freshtv142)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1562 "miniooMENHIR.ml"
        ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1566 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCATION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | ATOM | ELSE | EOF | EQUALS | FIELD _ | IDENT _ | IF | LCUR | LPAREN | LT | MALLOC | NULL | NUM _ | PARALLEL | PROCEDURE | RCUR | RPAREN | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv149 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1580 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1584 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (x : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1589 "miniooMENHIR.ml"
            ))), _, (e : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1593 "miniooMENHIR.ml"
            ))) = _menhir_stack in
            let _v : (
# 19 "miniooMENHIR.mly"
      ( ast )
# 1598 "miniooMENHIR.ml"
            ) = 
# 47 "miniooMENHIR.mly"
                              ( NewAssign(x, e) )
# 1602 "miniooMENHIR.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 19 "miniooMENHIR.mly"
      ( ast )
# 1610 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 19 "miniooMENHIR.mly"
      ( ast )
# 1618 "miniooMENHIR.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 19 "miniooMENHIR.mly"
      ( ast )
# 1626 "miniooMENHIR.ml"
            )) : (
# 19 "miniooMENHIR.mly"
      ( ast )
# 1630 "miniooMENHIR.ml"
            )) = _v in
            ((let _v : (
# 22 "miniooMENHIR.mly"
      ( ast )
# 1635 "miniooMENHIR.ml"
            ) = 
# 34 "miniooMENHIR.mly"
                ( _1 )
# 1639 "miniooMENHIR.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv151 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1649 "miniooMENHIR.ml"
            ))) * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1653 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 1666 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv39 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 1675 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 1684 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv43 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 1693 "miniooMENHIR.ml"
        )) * _menhir_state * (
# 22 "miniooMENHIR.mly"
      ( ast )
# 1697 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1706 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1715 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state) * _menhir_state * (
# 23 "miniooMENHIR.mly"
      ( ast )
# 1729 "miniooMENHIR.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1738 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1747 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1766 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * (
# 24 "miniooMENHIR.mly"
      ( ast )
# 1775 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1789 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv69 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1798 "miniooMENHIR.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv74)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FIELD _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NUM _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
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
# 1849 "miniooMENHIR.ml"
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
# 1860 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | LCUR ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | MALLOC ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
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
# 1900 "miniooMENHIR.ml"
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
# 21 "miniooMENHIR.mly"
      ( ast )
# 1921 "miniooMENHIR.ml"
    ) = 
# 51 "miniooMENHIR.mly"
                                  ( Skip )
# 1925 "miniooMENHIR.ml"
     in
    _menhir_goto_seqctrl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 13 "miniooMENHIR.mly"
       ( string )
# 1941 "miniooMENHIR.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv19 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1952 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | FIELD _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | LCUR ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | MALLOC ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | NUM _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | PROCEDURE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | SKIP ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | VAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv20)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv21 * _menhir_state) * (
# 13 "miniooMENHIR.mly"
       ( string )
# 1992 "miniooMENHIR.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "miniooMENHIR.mly"
       ( int )
# 2007 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : (
# 15 "miniooMENHIR.mly"
       ( int )
# 2017 "miniooMENHIR.ml"
    )) : (
# 15 "miniooMENHIR.mly"
       ( int )
# 2021 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 2026 "miniooMENHIR.ml"
    ) = 
# 73 "miniooMENHIR.mly"
                                  ( Num v )
# 2030 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv18)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 2043 "miniooMENHIR.ml"
    ) = 
# 78 "miniooMENHIR.mly"
                                  ( Empty )
# 2047 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv16)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FIELD _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | NUM _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | PROCEDURE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | FIELD _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LCUR ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | MALLOC ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NUM _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SKIP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | VAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | FIELD _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NUM _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "miniooMENHIR.mly"
       ( string )
# 2148 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 13 "miniooMENHIR.mly"
       ( string )
# 2158 "miniooMENHIR.ml"
    )) : (
# 13 "miniooMENHIR.mly"
       ( string )
# 2162 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 2167 "miniooMENHIR.ml"
    ) = 
# 69 "miniooMENHIR.mly"
                                  ( Ident (s) )
# 2171 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "miniooMENHIR.mly"
       ( string )
# 2178 "miniooMENHIR.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((f : (
# 14 "miniooMENHIR.mly"
       ( string )
# 2188 "miniooMENHIR.ml"
    )) : (
# 14 "miniooMENHIR.mly"
       ( string )
# 2192 "miniooMENHIR.ml"
    )) = _v in
    ((let _v : (
# 24 "miniooMENHIR.mly"
      ( ast )
# 2197 "miniooMENHIR.ml"
    ) = 
# 70 "miniooMENHIR.mly"
                                  ( Field (f) )
# 2201 "miniooMENHIR.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ATOM ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | FIELD _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | IF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LCUR ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | MALLOC ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NUM _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | PROCEDURE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | SKIP ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | VAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
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
       (MiniooAbstractSyntax.ast)
# 2268 "miniooMENHIR.ml"
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
    | ATOM ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FIELD _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LCUR ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MALLOC ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NUM _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | PROCEDURE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
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

# 86 "miniooMENHIR.mly"
   (* trailer *)

# 2314 "miniooMENHIR.ml"

# 269 "<standard.mly>"
  

# 2319 "miniooMENHIR.ml"
