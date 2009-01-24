
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

let parseSExprs source =
  let rec parse parseF (lexbuf :Lexing.lexbuf) codeAccum =
    try
      let expr = parseF lexbuf in
      parse parseF lexbuf (codeAccum @ [expr])
    with
        Sexprlexer.Eof | Indentlexer.Eof -> codeAccum
  in
  try
    let lexbuf = Lexing.from_string source in
    let parseF = Sexprparser.main Sexprlexer.token in
    let exprs = parse parseF lexbuf [] in
    Some exprs
  with _ ->
    None

let parseIExprsNoCatch source =
  let lexbuf = Lexing.from_string source in
  let lexstate = Indentlexer.lexbufFromString "dummy.zomp" source in
  let lexFunc _ = Indentlexer.token lexstate in
  let lexFunc = sampleFunc1 "lexing" lexFunc in
  let rec read acc =
    try
      let expr = Newparser.main lexFunc lexbuf in
      read (expr :: acc)
    with
      | Indentlexer.Eof -> acc
  in
  List.rev (read [])

let parseIExprs source =
  try
    Some (parseIExprsNoCatch source)
  with _ ->
    None

(** try to parse a string using indent/new syntax *)
let parseIExpr source =
  if String.length source >= 3 && Str.last_chars source 3 = "\n\n\n" then
    match parseIExprs source with
      | Some [singleExpr] ->
          Some singleExpr
      | Some multipleExprs ->
          Some { Ast2.id = "opseq"; args = multipleExprs }
      | None ->
          None
  else
    None

