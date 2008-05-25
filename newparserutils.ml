
let lexFunc lexstate (_ :Lexing.lexbuf) =
  let iexprToken = Indentlexer.token lexstate in
  match iexprToken with
    | `End -> Newparser.END
    | `BeginBlock -> Newparser.BEGIN_BLOCK
    | `EndBlock args -> Newparser.END_BLOCK args
    | `Identifier name -> Newparser.IDENTIFIER name
    | `OpenParen -> Newparser.OPEN_PAREN
    | `CloseParen -> Newparser.CLOSE_PAREN
    | `OpenCurlyBrackets -> Newparser.OPEN_CURLY
    | `CloseCurlyBrackets -> Newparser.CLOSE_CURLY
    | `Comma -> Newparser.COMMA
    | `Add arg -> Newparser.ADD_OP arg
    | `Mult arg -> Newparser.MULT_OP arg
    | `Assign arg -> Newparser.ASSIGN_OP arg
    | `Compare arg -> Newparser.COMPARE_OP arg
    | `Dot -> Newparser.DOT
    | `Postfix arg -> Newparser.POSTFIX_OP arg
    | `Prefix arg -> Newparser.PREFIX_OP arg
    | `LazyBoolOp arg -> Newparser.LAZY_BOOL_OP arg
    | `StrictBoolOp arg -> Newparser.STRICT_BOOL_OP arg
    | `Quote arg -> Newparser.QUOTE arg

