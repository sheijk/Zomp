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
    4
  end else begin
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
    exitCode
  end

let includePath = ref ["."]

let addIncludePath path = function
  | `Back ->
      includePath := !includePath @ [path]
  | `Front ->
      includePath := path :: !includePath

module CompilerInstructions =
struct
  open Expander
  open Ast2

  let clibPath = ref ["."; ".."]

  let translateLinkCLib env expr =
    collectTimingInfo "translateLinkCLib"
      (fun () ->
         match expr with
           | { args = [{id = fileName; args = []}] } ->
               begin
                 let fileName = Common.removeQuotes fileName in
                 let dllExtensions = ["dylib"; "so"; "dll"] in
                 let matches re string = Str.string_match (Str.regexp re) string 0 in
                 let dllPattern = sprintf ".*\\.\\(%s\\)" (Common.combine "\\|" dllExtensions) in
                 if not (matches dllPattern fileName) then
                   Error [sprintf "%s has invalid extension for a dll. Supported: %s"
                            fileName (Common.combine ", " dllExtensions)]
                 else
                   match Common.findFileIn fileName !clibPath with
                     | None ->
                         Error [sprintf "Could not find library '%s' in paths %s"
                                  fileName
                                  (Common.combine ", " !clibPath)]
                     | Some absoluteFileName ->
                         let handle = Zompvm.zompLoadLib absoluteFileName in
                         if handle = 0 then
                           Error [sprintf "Could not load C library '%s'\n" fileName]
                         else
                           Result (env.Expander.bindings, [])
               end
           | invalidExpr ->
               Error [sprintf "Expecting '%s fileName" invalidExpr.Ast2.id])
end

let () =
  at_exit Profiling.printTimings;
  match Sys.argv with
      [| execNameAndPath; "-c"; fileName |] ->
        begin
          match getBasename fileName with
            | Some baseName ->
                begin
                  let inputFileName = baseName ^ ".zomp" in
                  let outputFileName = baseName ^ ".ll" in
                  let inStream = open_in inputFileName
                  and outStream = open_out outputFileName
                  in
                  let compilerDir = Filename.dirname execNameAndPath in
                  addIncludePath compilerDir `Front;
                  let handleLLVMCode code = output_string outStream code in
                  let translateInclude = Expander.translateInclude includePath handleLLVMCode in
                  let addToplevelInstr = Hashtbl.add Expander.toplevelBaseInstructions in
                  addToplevelInstr "include" translateInclude;
                  addToplevelInstr "seq" (Expander.translateSeqTL handleLLVMCode);
                  addToplevelInstr "zmp:compiler:linkclib" CompilerInstructions.translateLinkCLib;
                  try
                    let exitCode = compile inputFileName inStream outStream in
                    close_in inStream;
                    close_out outStream;
                    if exitCode <> 0 then begin
                      eprintf "Failed to compile\n";
                      Sys.remove outputFileName;
                    end;
                    exit exitCode
                  with
                    | Sexprlexer.Eof ->
                        close_in inStream;
                        close_out outStream;
                        Sys.remove outputFileName
                    | _ as e ->
                        close_in inStream;
                        close_out outStream;
                        Sys.remove outputFileName;
                        raise e
                end
            | None ->
                reportError "Invalid file name. Expected *.zomp"
        end
    | _ ->
        printInstructions()

