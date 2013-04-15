
open Common
open Printf
open Basics

type parseError = {
  location :location option;
  reason :string;
}

let parseErrorToString pe =
  let file, line =
    match pe.location with
      | Some loc -> loc.fileName, loc.line
      | None -> "???.zomp", -1
  in
  sprintf "%s:%d: parsing error %s\n" file line pe.reason

let rec fixFileName fileName expr =
  let fixedArgs = List.map (fixFileName fileName) expr.Ast2.args in
  match expr.Ast2.location with
    | None ->
      { expr with Ast2.args = fixedArgs }
    | Some loc ->
      { expr with
        Ast2.location = Some { loc with fileName = fileName };
        Ast2.args = fixedArgs }

type parsingResult =
  | Exprs of Ast2.sexpr list
  | Error of parseError

let parseIExprsFromLexbuf lexbuf lexstate =
  let lexFunc lexbuf =
    let r = Indentlexer.token lexstate in
    let loc = Indentlexer.locationOfLexstate lexstate in
    let start = lexbuf.Lexing.lex_start_p in
    lexbuf.Lexing.lex_start_p <- { start with Lexing.pos_lnum = loc.line };
    r
  in
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
  with
    | Newparser.Error ->
      Error {
        location = Some (Indentlexer.locationOfLexstate lexstate);
        reason = "no parsing rule matched";
      }
    | Basics.ParseError (location, reason) ->
      Error { location = Some location; reason }
    | Indentlexer.UnknowToken (location, token, reason) ->
      Error {
        location = Some location;
        reason = Indentlexer.unknownTokenToErrorMsg (None, token, reason)
      }
    | Indentlexer.IndentError (location, msg) ->
      Error {
        location = Some location;
        reason = msg;
      }
    | exc ->
      Error {
        location = Some (Indentlexer.locationOfLexstate lexstate);
        reason = sprintf "unknown exception: %s" (Printexc.to_string exc)
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

