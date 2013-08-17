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
    pos_lnum = lineNum;
    pos_bol = columNum;
    pos_cnum = totalChars
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

let printInstructions() =
  printf "zompc -c fileName.zomp\n";
  printf "to compile fileName.zomp into fileName.ll\n"

let reportCommandLineArgumentError message =
  printf "error: %s\n" message;
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
  | Compilation_succeeded of Bindings.t
  | Compilation_failed of compilation_failure_reason

let compilation_result_to_int = function
  | Compilation_succeeded _ -> 0
  | Compilation_failed (Compilation_failed_with_error _) -> 1
  | Compilation_failed Compiler_did_not_return_result -> 2
  | Compilation_failed Failed_to_init_vm -> 4


let compile fileName inStream outStream =
  let preludeDir = Filename.dirname Sys.executable_name ^ "/../../../source" in
  let input =
    collectTimingInfo "reading prelude file content" (fun () -> Common.readChannel inStream)
  in

  if not( Zompvm.zompInit() ) then begin
    Compilation_failed Failed_to_init_vm
  end else begin
    Zompvm.zompVerifyCode false;
    let exitCode =
      Compileutils.catchingErrorsDo
        (fun () -> begin
          let preludeBindings :Bindings.t =
            Compileutils.loadPrelude
              ~processExpr:(fun expr oldBindings newBindings simpleforms llvmCode ->
                output_string outStream llvmCode)
              preludeDir
          in
          match Compileutils.compileCode preludeBindings input outStream fileName with
            | Some finalBindings -> Compilation_succeeded finalBindings
            | None -> Compilation_failed Compiler_did_not_return_result
        end)
        ~onErrors:(fun errors ->
          List.iter (printf "%s\n" ++ Serror.toString) errors;
          Compilation_failed (Compilation_failed_with_error "oops"))
    in
    Zompvm.zompShutdown();
    exitCode
  end

let includePath = ref []
let addIncludePath path where = addToList includePath path where

module CompilerInstructions =
struct
  open Expander
  open Ast2

  let clibPath = ref ["."; ".."; "./libs"; "./tools/external/lib"]
  let addDllPath path where = addToList clibPath path where

  let translateLinkCLib env expr =
    collectTimingInfo "translateLinkCLib"
      (fun () ->
        let location = someOrDefault expr.location Basics.fakeLocation in
         match expr with
           | { args = [{id = fileName; args = []}] } ->
               begin
                 let fileName = Common.removeQuotes fileName in
                 let dllExtensions = ["dylib"; "so"; "dll"] in
                 let matches re string = Str.string_match (Str.regexp re) string 0 in
                 let dllPattern = sprintf ".*\\.\\(%s\\)" (Common.combine "\\|" dllExtensions) in
                 if not (matches dllPattern fileName) then
                   Expander.errorFromString location
                     (sprintf "%s has invalid extension for a dynamic library. Supported: %s"
                            fileName (Common.combine ", " dllExtensions))
                 else
                   match Common.findFileIn fileName !clibPath with
                     | None ->
                         Expander.errorFromString location
                           (sprintf "could not find library '%s' in paths %s"
                              fileName
                              (Common.combine ", " (List.map (sprintf "\"%s\"") !clibPath)))
                     | Some absoluteFileName ->
                         let handle = Zompvm.zompLoadLib absoluteFileName in
                         if handle = 0 then
                           Expander.errorFromStringDeprecated
                             (sprintf "could not load C library '%s'\n" fileName)
                         else
                           Expander.tlReturnNoExprs env
               end
           | invalidExpr ->
               Expander.errorFromString location
                 (sprintf "expecting '%s fileName" invalidExpr.Ast2.id))
end

type options = {
  execNameAndPath :string;
  fileName :string;
  printTimings :bool;
  traceMacroExpansion :bool;
  symbolTableDumpFile :string option;
  zompIncludePaths :string list;
  dllPaths :string list;
}

type optionResult = Options of options | InvalidArguments of string

let extractOptions args =
  let addRelativePath list commandName dir =
    let absolutePath = Common.absolutePath dir in
    list := absolutePath :: !list;
    if not (Sys.file_exists absolutePath) then
      eprintf "warning: directory '%s' does not exist (%s)\n" absolutePath commandName
    else if not (Sys.is_directory absolutePath) then
      eprintf "warning: '%s' is not a directory (%s)\n" absolutePath commandName;
  in

  let fileName = ref "" in
  let printTimings = ref false in
  let traceMacroExpansion = ref false in
  let symbolTableDumpFile = ref "" in
  let zompIncludePaths = ref [] in
  let dllPaths = ref [] in
  let onAnonArg str =
    raise (Arg.Bad (sprintf "%s: anonymous arguments not supported" str))
  in
  try
    Arg.current := 0;
    Arg.parse_argv
      args
      ["--print-timings", Arg.Set printTimings, "print timing info on exit.";
       "--trace-macros", Arg.Set traceMacroExpansion, "Print trace information while expanding macros.";
       "-c", Arg.Set_string fileName, "The file to compile.";
       "--dump-symbols", Arg.Set_string symbolTableDumpFile, "A file to dump symbol table to.";
       "--zomp-include-dir",
         Arg.String (addRelativePath zompIncludePaths "--zomp-include-dir"),
         "A directory to be searched by include and requireLib";
       "--dll-dir",
         Arg.String (addRelativePath dllPaths "--dll-dir"),
         "A directory to be searched for dynamic libraries"]
      onAnonArg
      "zompc -c fileName.zomp\n";
    if (String.length !symbolTableDumpFile) > 0 then begin
      let absolutePath = Common.absolutePath !symbolTableDumpFile in
      symbolTableDumpFile := absolutePath;
    end;
    Options {
      execNameAndPath = args.(0);
      fileName = !fileName;
      printTimings = !printTimings;
      traceMacroExpansion = !traceMacroExpansion;
      symbolTableDumpFile = if String.length !symbolTableDumpFile = 0 then None else Some !symbolTableDumpFile;
      zompIncludePaths = !zompIncludePaths;
      dllPaths = !dllPaths;
    }
  with
    | Arg.Bad msg
    | Arg.Help msg ->
      InvalidArguments msg

let () =
  Printexc.record_backtrace true;
  let options =
    match extractOptions Sys.argv with
      | InvalidArguments msg ->
        printf "%s\n" msg;
        exit 1
      | Options options ->
        options
  in
  if options.printTimings then begin
    let printTimingStats() =
      Profiling.printTimings();
      Indentlexer.printStats();
      flush stdout;
      Zompvm.zompPrintStats();
    in
    at_exit printTimingStats;
  end;
  if options.traceMacroExpansion then begin
    let trace s e =
      if !Zompvm.traceMacroExpansionOn then begin
        let locString = sprintf "%s:%d" (Ast2.fileName e) (Ast2.lineNumber e) in
        printf "%s: Expansion step %s:\n%s\n" locString s (Ast2.toString e)
      end
    in
    Expander.setTraceMacroExpansion (Some trace);
  end;

  let baseName = match getBasename options.fileName with
    | Some baseName ->
      baseName
    | None ->
      reportCommandLineArgumentError "invalid file name. Expected *.zomp";
      exit 1
  in
  let inputFileName = canonicalFileName (baseName ^ ".zomp") in
  let outputFileName = canonicalFileName (baseName ^ ".ll") in

  let inStream = open_in inputFileName
  and outStream = open_out outputFileName
  in

  let compilerDir = Filename.dirname options.execNameAndPath in
  addIncludePath compilerDir `Front;
  addIncludePath (Sys.getcwd()) `Front;
  List.iter (fun dir -> addIncludePath dir `Front) options.zompIncludePaths;
  List.iter (fun dir -> CompilerInstructions.addDllPath dir `Front) options.dllPaths;

  let handleLLVMCode code = output_string outStream code in

  let addToplevelInstr = Expander.addToplevelInstruction in
  let translateInclude =
    Expander.makeTranslateIncludeFunction includePath handleLLVMCode
  in
  addToplevelInstr "include" "zompSourceFile" translateInclude;
  addToplevelInstr "seq" "ast..." (Expander.makeTranslateSeqFunction handleLLVMCode);
  addToplevelInstr "zmp:compiler:linkclib" "dllFileName"
    CompilerInstructions.translateLinkCLib;

  let exitCode =
    try
      compile inputFileName inStream outStream
    with _ as e ->
      Compilation_failed
        (Compilation_failed_with_error
           (sprintf "failed to compile due to unknown exception: %s\n%s\n"
              (Printexc.to_string e)
              (** returns backtrace of last thrown exception *)
              (Printexc.get_backtrace())))
  in

  close_in inStream;
  close_out outStream;

  begin match exitCode with
    | Compilation_succeeded globalBindings ->
      begin match options.symbolTableDumpFile with
        | Some absoluteFileName ->
          if not (Compileutils.writeSymbols globalBindings absoluteFileName) then
            printf "error: could not write symbols to '%s'\n" absoluteFileName;
        | None -> ();
      end
    | Compilation_failed reason ->
      eprintf "failed to compile: %s\n"
        begin match reason with
          | Failed_to_init_vm ->
            "could not init VM"
          | Compiler_did_not_return_result ->
            "compiler did not return a result"
          | Compilation_failed_with_error msg ->
            msg
        end;
      Sys.remove outputFileName;
  end;

  exit (compilation_result_to_int exitCode)

