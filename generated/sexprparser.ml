
# 1 "sexprparser.mly"
  
  exception ParseError of string

  let quoteId = function
    | "`" -> "quote"
    | "``" -> "quoteasis"
    | "#" -> "antiquote"
    | invalidString ->
        raise (ParseError
                 (Printf.sprintf
                    "Invalid quoting char: %s"
                    invalidString))

  let operatorName opSymbol = "op" ^ opSymbol
    

# 20 "sexprparser.ml"
exception Error

type token = 
  | QUOTE of (
# 20 "sexprparser.mly"
       (string)
# 27 "sexprparser.ml"
)
  | PAREN_OPEN
  | PAREN_CLOSE
  | MULT_OP of (
# 23 "sexprparser.mly"
       (string)
# 34 "sexprparser.ml"
)
  | IDENTIFIER of (
# 21 "sexprparser.mly"
       (string)
# 39 "sexprparser.ml"
)
  | COMPARE_OP of (
# 24 "sexprparser.mly"
       (string)
# 44 "sexprparser.ml"
)
  | ADD_OP of (
# 22 "sexprparser.mly"
       (string)
# 49 "sexprparser.ml"
)

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState0
  | MenhirState1
  | MenhirState2
  | MenhirState3
  | MenhirState4
  | MenhirState6
  | MenhirState8
  | MenhirState17
  | MenhirState21
  | MenhirState23
  | MenhirState25

let _eRR =
  Error

let rec _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_operatorExpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, e) = _menhir_stack in
    let _v : 'tv_operatorArg = 
# 66 "sexprparser.mly"
    ( e )
# 83 "sexprparser.ml"
     in
    _menhir_goto_operatorArg _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce4 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_operatorExpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 93 "sexprparser.ml"
    ) = 
# 38 "sexprparser.mly"
    ( e )
# 97 "sexprparser.ml"
     in
    _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_operatorExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_operatorExpr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | PAREN_CLOSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv115 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv116)
        | ADD_OP _ | COMPARE_OP _ | MULT_OP _ ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv117 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)
    | MenhirState25 | MenhirState23 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_operatorExpr) = Obj.magic _menhir_stack in
        (_menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) : 'freshtv124)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | PAREN_CLOSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv125 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = Obj.magic _menhir_stack in
            (_menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) : 'freshtv126)
        | ADD_OP _ | COMPARE_OP _ | MULT_OP _ ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv127 * _menhir_state) * _menhir_state * 'tv_operatorExpr) = _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)) : 'freshtv130)) : 'freshtv132)
    | _ ->
        _menhir_fail ()

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_operatorArg -> (
# 23 "sexprparser.mly"
       (string)
# 160 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv113 * _menhir_state * 'tv_operatorArg) * (
# 23 "sexprparser.mly"
       (string)
# 169 "sexprparser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | PAREN_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | QUOTE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error17 _menhir_env _menhir_stack) : 'freshtv114)

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_operatorArg -> (
# 22 "sexprparser.mly"
       (string)
# 187 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_operatorArg) * (
# 22 "sexprparser.mly"
       (string)
# 196 "sexprparser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | PAREN_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | QUOTE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error25 _menhir_env _menhir_stack) : 'freshtv112)

and _menhir_goto_operatorArg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_operatorArg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state * 'tv_operatorArg) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD_OP _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _v
        | COMPARE_OP _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv83 * _menhir_state) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
            let (_v : (
# 24 "sexprparser.mly"
       (string)
# 232 "sexprparser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv81 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 24 "sexprparser.mly"
       (string)
# 240 "sexprparser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | IDENTIFIER _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | PAREN_OPEN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | QUOTE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error23 _menhir_env _menhir_stack) : 'freshtv82)) : 'freshtv84)
        | MULT_OP _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv85 * _menhir_state) * _menhir_state * 'tv_operatorArg) = _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)) : 'freshtv90)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv93 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 23 "sexprparser.mly"
       (string)
# 268 "sexprparser.ml"
        )) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state * 'tv_operatorArg) * (
# 23 "sexprparser.mly"
       (string)
# 274 "sexprparser.ml"
        )) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, l), op), _, r) = _menhir_stack in
        let _v : 'tv_operatorExpr = 
# 59 "sexprparser.mly"
    ({ Ast2.id = operatorName op; args = [l; r] })
# 280 "sexprparser.ml"
         in
        _menhir_goto_operatorExpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv92)) : 'freshtv94)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv101 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 24 "sexprparser.mly"
       (string)
# 288 "sexprparser.ml"
        )) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv99 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 24 "sexprparser.mly"
       (string)
# 296 "sexprparser.ml"
        )) * _menhir_state * 'tv_operatorArg) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD_OP _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _v
        | MULT_OP _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v
        | COMPARE_OP _ | PAREN_CLOSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv95 * _menhir_state * 'tv_operatorArg) * (
# 24 "sexprparser.mly"
       (string)
# 309 "sexprparser.ml"
            )) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, l), op), _, r) = _menhir_stack in
            let _v : 'tv_operatorExpr = 
# 61 "sexprparser.mly"
    ({ Ast2.id = operatorName op; args = [l; r] })
# 315 "sexprparser.ml"
             in
            _menhir_goto_operatorExpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv96)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv97 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 24 "sexprparser.mly"
       (string)
# 325 "sexprparser.ml"
            )) * _menhir_state * 'tv_operatorArg) = _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)) : 'freshtv102)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv109 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 22 "sexprparser.mly"
       (string)
# 334 "sexprparser.ml"
        )) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv107 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 22 "sexprparser.mly"
       (string)
# 342 "sexprparser.ml"
        )) * _menhir_state * 'tv_operatorArg) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | MULT_OP _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v
        | ADD_OP _ | COMPARE_OP _ | PAREN_CLOSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv103 * _menhir_state * 'tv_operatorArg) * (
# 22 "sexprparser.mly"
       (string)
# 353 "sexprparser.ml"
            )) * _menhir_state * 'tv_operatorArg) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, l), op), _, r) = _menhir_stack in
            let _v : 'tv_operatorExpr = 
# 57 "sexprparser.mly"
    ({ Ast2.id = operatorName op; args = [l; r] })
# 359 "sexprparser.ml"
             in
            _menhir_goto_operatorExpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv104)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv105 * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 22 "sexprparser.mly"
       (string)
# 369 "sexprparser.ml"
            )) * _menhir_state * 'tv_operatorArg) = _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)) : 'freshtv110)
    | _ ->
        _menhir_fail ()

and _menhir_reduce8 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_sexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, e) = _menhir_stack in
    let _v : 'tv_operatorArg = 
# 64 "sexprparser.mly"
    ( e )
# 382 "sexprparser.ml"
     in
    _menhir_goto_operatorArg _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce3 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_sexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 392 "sexprparser.ml"
    ) = 
# 36 "sexprparser.mly"
    ( e )
# 396 "sexprparser.ml"
     in
    _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_sexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state) * _menhir_state * 'tv_sexpr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state) * _menhir_state * 'tv_sexpr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | PAREN_CLOSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv63 * _menhir_state) * _menhir_state * 'tv_sexpr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv64)
        | ADD_OP _ | COMPARE_OP _ | MULT_OP _ ->
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv65 * _menhir_state) * _menhir_state * 'tv_sexpr) = _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)) : 'freshtv70)
    | MenhirState25 | MenhirState23 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_sexpr) = Obj.magic _menhir_stack in
        (_menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) : 'freshtv72)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state) * _menhir_state * 'tv_sexpr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state) * _menhir_state * 'tv_sexpr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | PAREN_CLOSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv73 * _menhir_state) * _menhir_state * 'tv_sexpr) = Obj.magic _menhir_stack in
            (_menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) : 'freshtv74)
        | ADD_OP _ | COMPARE_OP _ | MULT_OP _ ->
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv75 * _menhir_state) * _menhir_state * 'tv_sexpr) = _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)) : 'freshtv78)) : 'freshtv80)
    | _ ->
        _menhir_fail ()

and _menhir_error25 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_operatorArg) * (
# 22 "sexprparser.mly"
       (string)
# 464 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error23 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * 'tv_operatorArg) * (
# 24 "sexprparser.mly"
       (string)
# 473 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error17 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_operatorArg) * (
# 23 "sexprparser.mly"
       (string)
# 482 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce6 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 491 "sexprparser.ml"
)) * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 495 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, q), _, e) = _menhir_stack in
    let _v : (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 502 "sexprparser.ml"
    ) = 
# 42 "sexprparser.mly"
    ({ Ast2.id = quoteId q; args = [e] })
# 506 "sexprparser.ml"
     in
    _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_sexprArg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_sexprArg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_sexprArg) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_sexprArg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_sexprArg) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_list_sexprArg_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_list_sexprArg_ = 
# 115 "standard.mly"
    ( x :: xs )
# 526 "sexprparser.ml"
         in
        _menhir_goto_list_sexprArg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv52)) : 'freshtv54)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * (
# 21 "sexprparser.mly"
       (string)
# 534 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_sexprArg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 21 "sexprparser.mly"
       (string)
# 542 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (args : 'tv_list_sexprArg_) = _v in
        ((let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : 'tv_sexpr = 
# 47 "sexprparser.mly"
    ({ Ast2.id = id; args = args })
# 550 "sexprparser.ml"
         in
        _menhir_goto_sexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv56)) : 'freshtv58)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state) * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 558 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_sexprArg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 566 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (args : 'tv_list_sexprArg_) = _v in
        ((let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : 'tv_sexpr = 
# 49 "sexprparser.mly"
    ({ Ast2.id = "seq"; args = id :: args })
# 574 "sexprparser.ml"
         in
        _menhir_goto_sexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv60)) : 'freshtv62)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sexprArg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sexprArg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_sexprArg) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_sexprArg) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | PAREN_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | QUOTE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | ADD_OP _ | COMPARE_OP _ | MULT_OP _ | PAREN_CLOSE ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv48)) : 'freshtv50)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState0 ->
        _menhir_error0 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState1 ->
        _menhir_error1 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState2 ->
        _menhir_error2 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState3 ->
        _menhir_error3 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState4 ->
        _menhir_error4 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state * (
# 21 "sexprparser.mly"
       (string)
# 618 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state * 'tv_sexprArg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState17 ->
        _menhir_error17 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 634 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState23 ->
        _menhir_error23 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState25 ->
        _menhir_error25 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error3 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 646 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error4 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_main : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 660 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 | MenhirState6 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 670 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 676 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : 'tv_sexprArg = 
# 54 "sexprparser.mly"
    ( e )
# 682 "sexprparser.ml"
         in
        _menhir_goto_sexprArg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv26)) : 'freshtv28)
    | MenhirState2 | MenhirState4 | MenhirState23 | MenhirState25 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 690 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 698 "sexprparser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENTIFIER _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | PAREN_OPEN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | QUOTE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | ADD_OP _ | COMPARE_OP _ | MULT_OP _ | PAREN_CLOSE ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv30)) : 'freshtv32)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 715 "sexprparser.ml"
        )) * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 719 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        (_menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) : 'freshtv34)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 727 "sexprparser.ml"
        )) * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 731 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        (_menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) : 'freshtv36)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 739 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state * (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 745 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        Obj.magic _1) : 'freshtv38)) : 'freshtv40)

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_sexprArg_ = 
# 113 "standard.mly"
    ( [] )
# 755 "sexprparser.ml"
     in
    _menhir_goto_list_sexprArg_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "sexprparser.mly"
       (string)
# 762 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv23) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (id : (
# 21 "sexprparser.mly"
       (string)
# 772 "sexprparser.ml"
    )) = _v in
    ((let _v : 'tv_sexprArg = 
# 52 "sexprparser.mly"
    ({ Ast2.id = id; args = [] })
# 777 "sexprparser.ml"
     in
    _menhir_goto_sexprArg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv24)

and _menhir_error1 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 784 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce7 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 793 "sexprparser.ml"
) -> _menhir_state -> (
# 21 "sexprparser.mly"
       (string)
# 797 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ e ->
    let (_menhir_stack, _menhir_s, q) = _menhir_stack in
    let _v : (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 804 "sexprparser.ml"
    ) = 
# 44 "sexprparser.mly"
    ({ Ast2.id = quoteId q; args = [{Ast2.id = e; args = []}] })
# 808 "sexprparser.ml"
     in
    _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v

and _menhir_error2 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "sexprparser.mly"
       (string)
# 820 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 829 "sexprparser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 838 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState3 in
        let (_v : (
# 21 "sexprparser.mly"
       (string)
# 844 "sexprparser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv20)
    | PAREN_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | QUOTE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error3 _menhir_env _menhir_stack) : 'freshtv22)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | PAREN_CLOSE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState4 in
        ((let _ = _menhir_discard _menhir_env in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | PAREN_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | QUOTE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error4 _menhir_env _menhir_stack) : 'freshtv18)

and _menhir_reduce5 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 888 "sexprparser.ml"
    ) = 
# 40 "sexprparser.mly"
    ({ Ast2.id = "seq"; args = [] })
# 892 "sexprparser.ml"
     in
    _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "sexprparser.mly"
       (string)
# 899 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 21 "sexprparser.mly"
       (string)
# 908 "sexprparser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | PAREN_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | QUOTE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | ADD_OP _ | COMPARE_OP _ | MULT_OP _ | PAREN_CLOSE ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv14)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_error0 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "sexprparser.mly"
       (string)
# 940 "sexprparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 949 "sexprparser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state * (
# 20 "sexprparser.mly"
       (string)
# 958 "sexprparser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState1 in
        let (_v : (
# 21 "sexprparser.mly"
       (string)
# 964 "sexprparser.ml"
        )) = _v in
        (_menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv10)
    | PAREN_OPEN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | QUOTE _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error1 _menhir_env _menhir_stack) : 'freshtv12)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | PAREN_CLOSE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState2 in
        (_menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv6)
    | PAREN_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | QUOTE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error2 _menhir_env _menhir_stack) : 'freshtv8)

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 30 "sexprparser.mly"
       (Ast2.sexpr)
# 1003 "sexprparser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = 1073741823
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | PAREN_OPEN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | QUOTE _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error0 _menhir_env _menhir_stack) : 'freshtv2)) : 'freshtv4))



