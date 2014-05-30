
open Common
open Printf
open Basics

let rec fixFileName fileName expr =
  let fixedArgs = List.map (fixFileName fileName) expr.Ast2.args in
  match expr.Ast2.location with
    | None ->
      expr >>= Ast2.withArgs fixedArgs
    | Some loc ->
      expr >>=
        Ast2.withLoc { loc with fileName = fileName } >>=
        Ast2.withArgs fixedArgs

type parsingResult =
  | Exprs of Ast2.sexpr list
  | Error of Serror.t

let parseIExprsFromLexbuf lexbuf lexstate lineCount : Ast2.t list =
  let lexFunc lexbuf =
    let r = Indentlexer.token lexstate in
    let loc = Indentlexer.locationOfLexstate lexstate in
    let start = lexbuf.Lexing.lex_start_p in
    lexbuf.Lexing.lex_start_p <- { start with Lexing.pos_lnum = loc.line };
    r
  in
  try
    let lexFunc = sampleFunc1 "lexing" lexFunc in
    let exprs = Newparser.main lexFunc lexbuf in
    exprs
  with ParseError (loc, msg) as exn ->
    if loc.line > lineCount then
      raise (ParseError ({ loc with line = loc.line - 1 }, msg))
    else
      raise exn

let createLexState ~fileName source =
  let open Lexing in
  let lexbuf =
    let lexbufNoLoc = Lexing.from_string source in
    { lexbufNoLoc with
      lex_start_p = { lexbufNoLoc.lex_start_p with pos_fname = fileName };
      lex_curr_p = { lexbufNoLoc.lex_curr_p with pos_fname = fileName } }
  in
  let lexstate = Indentlexer.lexbufFromString ~fileName source in
  lexbuf, lexstate

let parseIExprs ~fileName source : parsingResult =
  let lexbuf, lexstate = createLexState ~fileName source in
  try
    Exprs (parseIExprsFromLexbuf lexbuf lexstate (lineCount source))
  with
    | Newparser.Error ->
      Error (Serror.fromMsg
               (Some (Indentlexer.locationOfLexstate lexstate))
               "no parsing rule matched")
    | Basics.ParseError (location, reason) ->
      Error (Serror.fromMsg (Some location) reason)
    | Indentlexer.UnknowToken (location, token, reason) ->
      Error (Serror.fromMsg
               (Some location)
               (Indentlexer.unknownTokenToErrorMsg (None, token, reason)))
    | Indentlexer.IndentError (location, msg) ->
      Error (Serror.fromMsg (Some location) msg)
    | exn ->
      Error (Serror.fromException 
               (Some (Indentlexer.locationOfLexstate lexstate))
               exn)

(** Used in legacy code in Expander *)
let parseIExprsOpt ~fileName source : Ast2.t list option =
  match parseIExprs ~fileName source with
    | Exprs e -> Some e
    | Error _ -> None

(** try to parse a string using indent/new syntax *)
let parseIExpr ~fileName source : Ast2.t option =
  if String.length source >= 3 && Str.last_chars source 3 = "\n\n\n" then
    match parseIExprs ~fileName source with
      | Exprs [singleExpr] ->
          Some singleExpr
      | Exprs multipleExprs ->
          Some (Ast2.seqExpr multipleExprs)
      | Error _ ->
          None
  else
    None

