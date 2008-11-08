
open Common
open Printf

let compileExpr translateF bindings sexpr =
  let newBindings, simpleforms =
    collectTimingInfo "generating ast"
      (fun () -> translateF bindings sexpr)
  in
  let llvmCodes =
    collectTimingInfo "codegen"
      (fun () -> List.map Genllvm.gencodeTL simpleforms)
  in
  let llvmCode = combine "\n" llvmCodes in
  newBindings, simpleforms, llvmCode

(** try to parse a string using s-expr syntax *)
let parseSExpr input =
  try
    let lexbuf = Lexing.from_string input in
    Some( Sexprparser.main Sexprlexer.token lexbuf )
  with Sexprlexer.Eof ->
    None

(** try to parse a string using indent/new syntax *)
let parseIExpr source =
  if String.length source >= 3 && Str.last_chars source 3 = "\n\n\n" then
    try
      let lexbuf = Lexing.from_string source in
      let lexstate = Indentlexer.lexbufFromString "dummy.zomp" source in
      let lexFunc _ = Indentlexer.token lexstate in
      let rec read acc =
        try
          let expr = Newparser.main lexFunc lexbuf in
          read (expr :: acc)
        with
          | Indentlexer.Eof -> acc
      in
      match List.rev (read []) with
        | [singleExpr] ->
            Some singleExpr
        | multipleExprs ->
            Some { Ast2.id = "opseq"; args = multipleExprs }
    with _ ->
      None
  else (
    None)

    
