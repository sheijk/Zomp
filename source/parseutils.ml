
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
        reason = "No rule matched";
      }
    | exc ->
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

