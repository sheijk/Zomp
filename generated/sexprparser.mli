exception Error

type token = 
  | QUOTE of (string)
  | PAREN_OPEN
  | PAREN_CLOSE
  | MULT_OP of (string)
  | IDENTIFIER of (string)
  | COMPARE_OP of (string)
  | ADD_OP of (string)


val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast2.sexpr)