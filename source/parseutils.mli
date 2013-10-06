val fixFileName : string -> Ast2.sexpr -> Ast2.sexpr
type parsingResult = Exprs of Ast2.sexpr list | Error of Serror.t
val parseIExprsFromLexbuf :
  Lexing.lexbuf ->
  Newparser.token Indentlexer.lexerstate -> int -> Ast2.t list
val createLexState :
  fileName:string -> string -> Lexing.lexbuf * 'a Indentlexer.lexerstate
val parseIExprsNoCatch : fileName:string -> string -> Ast2.t list
val parseIExprs : fileName:string -> string -> parsingResult
val parseIExprsOpt : fileName:string -> string -> Ast2.t list option
val parseIExpr : fileName:string -> string -> Ast2.t option
