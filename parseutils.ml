
open Common
open Printf
  
let rec parse parseF (lexbuf :Lexing.lexbuf) bindings codeAccum =
  try
    let expr = parseF lexbuf in
(*     let exprString = Ast2.expression2string expr in *)
(*     let exprComment = commentOut "; " exprString in *)
(*     printf "%s\n" exprComment; *)
    let newBindings, simpleforms = Expander.translateTL bindings expr in
    parse parseF lexbuf newBindings (codeAccum @ simpleforms)
  with
    | Lexer2.Eof | Sexprlexer.Eof -> bindings, codeAccum

