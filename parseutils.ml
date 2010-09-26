
open Common
open Printf

(** try to parse a string using s-expr syntax *)
let parseSExpr input =
  try
    let lexbuf = Lexing.from_string input in
    Some( Sexprparser.main Sexprlexer.token lexbuf )
  with Sexprlexer.Eof ->
    None

type parseError = {
  location :Indentlexer.location option;
  reason :string;
}

let parseErrorToString pe =
  let file, line =
    match pe.location with
      | Some loc -> loc.Indentlexer.fileName, loc.Indentlexer.line
      | None -> "???.zomp", -1
  in
  sprintf "%s:%d: parsing error %s\n" file line pe.reason

type parsingResult =
  | Exprs of Ast2.sexpr list
  | Error of parseError

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
    Exprs exprs
  with exc ->
    Error {
      location = None;
      reason = sprintf "Unknow exception: %s" (Printexc.to_string exc)
    }

let parseIExprsFromLexbuf lexbuf lexstate =
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

let parseIExprsNoCatch source =
  let lexbuf = Lexing.from_string source in
  let lexstate = Indentlexer.lexbufFromString "dummy.zomp" source in
  parseIExprsFromLexbuf lexbuf lexstate

let parseIExprs source =
  let lexbuf = Lexing.from_string source in
  let lexstate = Indentlexer.lexbufFromString "dummy.zomp" source in
  try
    Exprs (parseIExprsFromLexbuf lexbuf lexstate)
  with exc ->
    Error {
      location = Some (Indentlexer.locationOfLexstate lexstate);
      reason = sprintf "Unknow exception: %s" (Printexc.to_string exc)
    }

let parseIExprsOpt source =
  match parseIExprs source with
    | Exprs e -> Some e
    | Error _ -> None

(** try to parse a string using indent/new syntax *)
let parseIExpr source =
  if String.length source >= 3 && Str.last_chars source 3 = "\n\n\n" then
    match parseIExprs source with
      | Exprs [singleExpr] ->
          Some singleExpr
      | Exprs multipleExprs ->
          Some (Ast2.seqExpr multipleExprs)
      | Error _ ->
          None
  else
    None

