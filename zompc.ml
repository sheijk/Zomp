open Ast2
open Printf
open Expander
open Genllvm
open Common
open Parseutils
open Compileutils

type llvmCode = string

(* type compilationResult = *)
(*   | CompilationSucceeded of llvmCode *)
(*   | CouldNotParse of string *)
(*   | CouldNotCompile of string *)

type sourceloc = {
  fileName :string;
  line :int;
  column :int;
  charsFromBeginning: int;
}

let locationFromLexbuf lexbuf =
  let {
    Lexing.pos_fname = fileName;
    Lexing.pos_lnum = lineNum;
    Lexing.pos_bol = columNum;
    Lexing.pos_cnum = totalChars
  } = lexbuf.Lexing.lex_curr_p
  in
  let fileName = if String.length fileName > 0 then fileName else "dummy.zomp" in
  (* TODO: update pos_lnum (and pos_fname) in lexer *)
  {
    fileName = fileName;
    line = lineNum;
    column = columNum;
    charsFromBeginning = totalChars;
  }

let parseChannel lexbuf errorLocationF parseF bindings =
  try
    let newBindings, toplevelExprs =
      collectTimingInfo "building ast" (fun () -> parse parseF lexbuf bindings [] )
    in
    let llvmSource :string =
      collectTimingInfo "code generation" (fun () -> genmodule toplevelExprs)
    in
    newBindings, toplevelExprs, llvmSource
  with
    | Sexprparser.Error ->
        begin
          let loc = errorLocationF lexbuf in
          raiseCouldNotParse
            (sprintf "%s:%d:%d: error: could not parse %d chars from beginning of file\n"
               loc.fileName loc.line loc.column loc.charsFromBeginning)
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

let compile fileName instream outstream =
  let preludeDir = Filename.dirname Sys.executable_name in
  let input =
    collectTimingInfo "reading prelude file content" (fun () -> readInput instream)
  in

  if not( Zompvm.zompInit() ) then begin
    eprintf "Could not init ZompVM\n";
    exit 3;
  end;
  Zompvm.zompVerifyCode false;
  let exitCode =
    Compileutils.catchingErrorsDo
      (fun () -> begin
         let bindings :Bindings.t =
           Compileutils.loadPrelude
             ~dir:preludeDir
             ~processExpr:(fun expr oldBindings newBindings simpleforms llvmCode ->
                             output_string outstream llvmCode)
         in
         match Compileutils.compileCode bindings input outstream fileName with
           | Some _ -> 0
           | None -> 2
       end)
      ~onError:(fun msg ->
                  printf "Failed compilation: %s" msg;
                  1)
  in
  Zompvm.zompShutdown();
  if exitCode <> 0 then
    eprintf "Failed to compile\n";
  exit exitCode

let includePath = ref ["."]

let addIncludePath path = function
  | `Back ->
      includePath := !includePath @ [path]
  | `Front ->
      includePath := path :: !includePath

let () =
  match Sys.argv with
      [| execNameAndPath; "-c"; fileName |] ->
        begin
          match getBasename fileName with
            | Some baseName ->
                begin
                  let inStream = open_in (baseName ^ ".zomp")
                  and outStream = open_out (baseName ^ ".ll")
                  in
                  let compilerDir = Filename.dirname execNameAndPath in
                  addIncludePath compilerDir `Front;
                  let handleLLVMCode code = output_string outStream code in
                  let translateInclude = Expander.translateInclude includePath handleLLVMCode in
                  Hashtbl.add Expander.toplevelBaseInstructions "include" translateInclude;
                  Hashtbl.add Expander.toplevelBaseInstructions "seq"
                    (Expander.translateSeqTL handleLLVMCode);
                  try
                    compile (baseName ^ ".zomp") inStream outStream
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

