open Ast2
open Printf
open Expander
open Genllvm
open Common
open Parseutils

type llvmCode = string
    
(* type compilationResult = *)
(*   | CompilationSucceeded of llvmCode *)
(*   | CouldNotParse of string *)
(*   | CouldNotCompile of string *)

exception CouldNotParse of string
let raiseCouldNotParse str = raise (CouldNotParse str)
  
exception CouldNotCompile of string
let raiseCouldNotCompile str = raise (CouldNotCompile str)
  
let parseChannel lexbuf parseF bindings =
  try
    let newBindings, toplevelExprs = parse parseF lexbuf bindings [] in
    let llvmSource :string = genmodule toplevelExprs in
    newBindings, llvmSource
  with
    | Parser2.Error
    | Sexprparser.Error ->
      begin
        let {
          Lexing.pos_fname = fileName;
          Lexing.pos_lnum = lineNum;
          Lexing.pos_bol = columNum;
          Lexing.pos_cnum = totalChars
        } = lexbuf.Lexing.lex_curr_p
        in
        let fileName = if String.length fileName > 0 then fileName else "dummy.zomp" in
        raiseCouldNotParse
          (sprintf "%s:%d:%d: error: could not parse %d chars from beginning of file\n"
             fileName lineNum columNum totalChars)
          (* TODO: update pos_lnum (and pos_fname) in lexer *)
      end
  | Expander.IllegalExpression (expr, msg) ->
      begin
        raiseCouldNotCompile
          (sprintf "Error expanding to canonical simpleform in expression:\n%s\n\nMessage: %s\n"
             (Ast2.expression2string expr) msg)
      end
          
let rec readInput str =
  try readInput (str ^ read_line() ^ "\n")
  with End_of_file -> str
          
let () =
  let preludeFileName = Filename.dirname Sys.executable_name ^ "/stdlib.zomp" in
  let prelude = Common.readFile preludeFileName in
  let input = readInput "" in
  let printError = function
    | CouldNotParse msg -> eprintf "%s" msg
    | CouldNotCompile msg -> eprintf "%s" msg
    | unknownError -> eprintf "Unknown error: %s\n" (Printexc.to_string unknownError)
  in
  let compile bindings input =
    let parseF lexbuf = parseChannel (Lexing.from_string input) lexbuf bindings in
    tryAllCollectingErrors
      [
        lazy (parseF (Parser2.main Lexer2.token));
        lazy (parseF (Sexprparser.main Sexprlexer.token));
      ]
      ~onSuccess:(fun (newBindings, llvmCode) -> printf "%s" llvmCode; Some newBindings)
      ~ifAllFailed:(fun exceptions -> List.iter printError exceptions; None)
  in
  match compile Genllvm.defaultBindings prelude with
    | Some bindings ->
        begin match compile bindings input with
          | Some _ -> exit 0
          | None -> exit 2
        end
    | None ->
        exit 1

