open Ast2
open Printf
open Expander
open Genllvm
open Common
open Parseutils
open Compileutils

type llvmCode = string

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

let reportCommandLineArgumentError message =
  printf "Error: %s\n" message;
  printInstructions()

let getBasename filename =
  let zompFileRE = Str.regexp "\\(.+\\)\\.zomp" in
  if Str.string_match zompFileRE filename 0 then
    Some (Str.matched_group 1 filename)
  else
    None

type compilation_failure_reason =
  | Compiler_did_not_return_result
  | Compilation_failed_with_error of string
  | Failed_to_init_vm

type compilation_result =
  | Compilation_succeeded
  | Compilation_failed of compilation_failure_reason

let compilation_result_to_int = function
  | Compilation_succeeded -> 0
  | Compilation_failed (Compilation_failed_with_error _) -> 1
  | Compilation_failed Compiler_did_not_return_result -> 2
  | Compilation_failed Failed_to_init_vm -> 4

let compile fileName instream outstream =
  let preludeDir = Filename.dirname Sys.executable_name in
  let input =
    collectTimingInfo "reading prelude file content" (fun () -> readInput instream)
  in

  if not( Zompvm.zompInit() ) then begin
    Compilation_failed Failed_to_init_vm
  end else begin
    Zompvm.zompVerifyCode false;
    let exitCode =
      Compileutils.catchingErrorsDo
        (fun () -> begin
           let bindings :Bindings.t =
             Compileutils.loadPrelude
               ~processExpr:(fun expr oldBindings newBindings simpleforms llvmCode ->
                               output_string outstream llvmCode)
               preludeDir
           in
           match Compileutils.compileCode bindings input outstream fileName with
             | Some _ -> Compilation_succeeded
             | None -> Compilation_failed Compiler_did_not_return_result
         end)
        ~onError:(fun msg -> Compilation_failed (Compilation_failed_with_error msg))
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
                   Expander.errorFromString
                     (sprintf "%s has invalid extension for a dll. Supported: %s"
                            fileName (Common.combine ", " dllExtensions))
                 else
                   match Common.findFileIn fileName !clibPath with
                     | None ->
                         Expander.errorFromString
                           (sprintf "Could not find library '%s' in paths %s"
                              fileName
                              (Common.combine ", " !clibPath))
                     | Some absoluteFileName ->
                         let handle = Zompvm.zompLoadLib absoluteFileName in
                         if handle = 0 then
                           Expander.errorFromString
                             (sprintf "Could not load C library '%s'\n" fileName)
                         else
                           Expander.tlReturnNoExprs env
               end
           | invalidExpr ->
               Expander.errorFromString
                 (sprintf "Expecting '%s fileName" invalidExpr.Ast2.id))
end

type options = {
  execNameAndPath :string;
  fileName :string;
  printTimings :bool;
}

let extractOptions = function
  | [| execNameAndPath; "-c"; fileName |] ->
      Some {
        execNameAndPath = execNameAndPath;
        fileName = fileName;
        printTimings = false;
      }
  | [| execNameAndPath; "-c"; fileName; "--print-timings" |] ->
      Some {
        execNameAndPath = execNameAndPath;
        fileName = fileName;
        printTimings = true;
      }
  | _ ->
      None

let () =
  Printexc.record_backtrace true;
  match extractOptions Sys.argv with
    | Some options ->
        if options.printTimings then
          at_exit (fun () ->
                     Profiling.printTimings();
                     Indentlexer.printStats();
                     flush stdout;
                     Zompvm.zompPrintStats());
        begin
          match getBasename options.fileName with
            | Some baseName ->
                begin
                  let inputFileName = baseName ^ ".zomp" in
                  let outputFileName = baseName ^ ".ll" in
                  let inStream = open_in inputFileName
                  and outStream = open_out outputFileName
                  in
                  let compilerDir = Filename.dirname options.execNameAndPath in
                  addIncludePath compilerDir `Front;
                  let handleLLVMCode code = output_string outStream code in
                  let translateInclude =
                    Expander.makeTranslateIncludeFunction includePath handleLLVMCode
                  in
                  let addToplevelInstr = Expander.addToplevelInstruction in
                  addToplevelInstr "include" translateInclude;
                  addToplevelInstr "seq" (Expander.makeTranslateSeqFunction handleLLVMCode);
                  addToplevelInstr "zmp:compiler:linkclib" CompilerInstructions.translateLinkCLib;
                  try
                    let exitCode = compile inputFileName inStream outStream in
                    close_in inStream;
                    close_out outStream;
                    begin match exitCode with
                      | Compilation_succeeded -> ()
                      | Compilation_failed reason ->
                          eprintf "Failed to compile: %s\n"
                            begin match reason with
                              | Failed_to_init_vm -> "Could to init VM"
                              | Compiler_did_not_return_result -> "Compiler did not return a result"
                              | Compilation_failed_with_error msg -> msg
                            end;
                          Sys.remove outputFileName;
                    end;
                    exit (compilation_result_to_int exitCode)
                  with
                    | _ as e ->
                        close_in inStream;
                        close_out outStream;
                        Sys.remove outputFileName;
                        eprintf "Failed to compile due to unknow exception\n";
                        Printexc.print_backtrace stdout;
                        raise e
                end
            | None ->
                reportCommandLineArgumentError "Invalid file name. Expected *.zomp"
        end
    | None ->
        printInstructions()

