open Ast2
open Printf
open Expander
open Genllvm
open Common
open Parseutils
open Compileutils

let section = Statistics.createSection "zompc"

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

let preludeTime = ref 0.0
let mainFileTime = ref 0.0

let () =
  Statistics.createFloatCounter section "compile prelude (s)" 3 (Ref.getter preludeTime);
  Statistics.createFloatCounter section "compile main file (s)" 3 (Ref.getter mainFileTime);
  ()

let compile fileName inStream outStream =
  let preludeDir = Filename.dirname Sys.executable_name ^ "/../../../source" in
  let input =
    collectTimingInfo "reading prelude file content" (fun () -> Common.readChannel inStream)
  in

  let emitBackendCode source =
    output_string outStream source
  in
  let reportDiagnostics diag =
    eprintf "%s\n" $ Serror.toString diag;
    flush stderr
  in

  if not( Zompvm.zompInit() ) then begin
    Compilation_failed Failed_to_init_vm
  end else begin
    Zompvm.zompVerifyCode false;
    let env = Compileutils.createEnv Genllvm.defaultBindings in
    let exitCode =
      let preludeResult = addTiming preludeTime $ fun () -> Compileutils.loadPrelude env ~emitBackendCode preludeDir in
      List.iter reportDiagnostics preludeResult.Result.diagnostics;
      if preludeResult.Result.flag = Result.Fail then
        Compilation_failed (Compilation_failed_with_error "failed to compile prelude")
      else begin
        addTiming mainFileTime $ fun () ->
          let { Result.flag; diagnostics; _ } =
            Compileutils.compileFromStream env ~source:input ~emitBackendCode ~fileName
          in
          List.iter reportDiagnostics diagnostics;
          if flag = Result.Success then
            Compilation_succeeded (Compileutils.bindings env)
          else
            Compilation_failed Compiler_did_not_return_result
      end
    in
    Zompvm.zompShutdown();
    exitCode
  end

let includePath = ref []
let addIncludePath path where = addToList includePath path where

let dllPath = ref ["."; ".."; "./libs"; "./tools/external/lib"]
let addDllPath path where = addToList dllPath path where

type outputTarget = [`ToFile of string | `Stdout | `DoNotWrite]
let outputTargetFromString = function
  | "" -> `DoNotWrite
  | "-" -> `Stdout
  | file when file.[0] = '-' -> raise (Arg.Bad (sprintf "file name '%s' invalid, must not start with -" file))
  | file -> `ToFile file

type options = {
  execNameAndPath :string;
  fileName :string;
  printTimings :bool;
  printStats :outputTarget;
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
  let printStats = ref "" in
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
       "--stats", Arg.Set_string printStats, "print statistics on exit.";
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
      printStats = outputTargetFromString !printStats;
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
  let startTime = Sys.time() in

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
      flush stdout;
      Zompvm.zompPrintTimingStats();
    in
    at_exit printTimingStats;
  end;
  begin match options.printStats with
    | `DoNotWrite -> ()
    | `Stdout -> at_exit (fun () -> Stats.statsPrintReport 0)
    | `ToFile file -> at_exit (fun () ->
      if not (Stats.statsPrintReportToFile file 0) then
        eprintf "error: could not write statistics to file %s\n" file)
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
  List.iter (fun dir -> addDllPath dir `Front) options.dllPaths;

  let handleLLVMCode code = output_string outStream code in

  let addToplevelInstr = Expander.addToplevelInstruction in
  let translateInclude =
    Expander.makeTranslateIncludeFunction includePath handleLLVMCode
  in
  addToplevelInstr "include" "zompSourceFile" translateInclude;
  addToplevelInstr "seq" "ast..." (Expander.makeTranslateSeqFunction handleLLVMCode);
  addToplevelInstr "zmp:compiler:linkclib" "dllFileName" (Expander.translateLinkCLib dllPath);

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
            eprintf "error: could not write symbols to '%s'\n" absoluteFileName;
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

  let endTime = Sys.time() in
  Statistics.createFloatCounter section "compilation time (s)" 3 (fun () -> endTime -. startTime);

  exit (compilation_result_to_int exitCode)

