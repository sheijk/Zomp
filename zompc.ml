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
    newBindings, toplevelExprs, llvmSource
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
    | Common.FailedToEvaluateLLVMCode (code, msg) ->
        begin
          raiseCouldNotCompile
            (sprintf "LLVM error when compiling macro: %s\n%s\n" msg code)
        end

let readInput = Common.readChannel
  
let printInstructions() =
  printf "zompc -c fileName.zomp\n";
  printf "to compile fileName.zomp into fileName.ll\n"

let reportError message =
  printf "Error: %s\n" message;
  printInstructions()

let getBasename filename =
  let zompFileRE = Str.regexp "\\(.+\\)\\.zomp" in
  if Str.string_match zompFileRE filename 0 then
    Some (Str.matched_group 1 filename)
  else
    None

let compile instream outstream =
  let preludeDir = Filename.dirname Sys.executable_name in
  let input = readInput instream in
  let printError = function
    | CouldNotParse msg -> eprintf "%s" msg
    | CouldNotCompile msg -> eprintf "%s" msg
    | unknownError -> eprintf "Unknown error: %s\n" (Printexc.to_string unknownError); raise unknownError
  in
  let compileCode bindings input =
    let parseF lexbuf = parseChannel (Lexing.from_string input) lexbuf bindings in
    tryAllCollectingErrors
      [
        lazy (parseF (Parser2.main Lexer2.token));
        lazy (parseF (Sexprparser.main Sexprlexer.token));
      ]
      ~onSuccess:(fun (newBindings, toplevelExprs, llvmCode) ->
                    output_string outstream llvmCode;
                    Some newBindings)
      ~ifAllFailed:(fun exceptions -> List.iter printError exceptions; None)
  in
  if not( Zompvm.zompInit() ) then begin
    eprintf "Could not init ZompVM\n";
    exit 3;
  end;
  let exitCode =
    Parseutils.catchingErrorsDo
      (fun () ->
         let bindings = Parseutils.loadPrelude
           ~dir:preludeDir
           ~processExpr:(fun expr oldBindings newBindings simpleforms llvmCode ->
                           output_string outstream llvmCode
                        )
         in
         match compileCode bindings input with
           | Some _ -> 0
           | None -> 2)
      ~onError:(fun () -> 1)
  in
  Zompvm.zompShutdown();
  exit exitCode

let () =
  match Sys.argv with
      [| _; "-c"; fileName |] ->
        begin
          match getBasename fileName with
            | Some baseName ->
                begin
                  let inStream = open_in (baseName ^ ".zomp")
                  and outStream = open_out (baseName ^ ".ll")
                  in
                  try
                    compile inStream outStream
                  with
                    | Sexprlexer.Eof ->
                        close_in inStream;
                        close_out outStream;
                    | _ as e ->
                        close_in inStream;
                        close_out outStream;
                        Sys.remove (baseName ^ ".ll");
                        raise e
                end
            | None ->
                reportError "Invalid file name. Expected *.zomp"
        end
    | _ ->
        printInstructions()
    
