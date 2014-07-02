open Ast2
open Printf
open Expander
open Genllvm
open Common
open Parseutils
open Compileutils

module Options = struct
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
    traceBaseForms :bool;
    traceLlvmCode :bool;
    symbolTableDumpFile :string option;
    zompIncludePaths :string list;
    dllPaths :string list;
  }

  type optionResult = Options of options | InvalidArguments of string

  let extract args =
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
    let traceBaseForms = ref false in
    let traceLlvmCode = ref false in
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
         "--trace-definitions", Arg.Set traceBaseForms, "Print every toplevel definition.";
         "--trace-llvm-code", Arg.Set traceLlvmCode, "Trace emitted LLVM code.";
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
        traceBaseForms = !traceBaseForms;
        traceLlvmCode = !traceLlvmCode;
        symbolTableDumpFile = if String.length !symbolTableDumpFile = 0 then None else Some !symbolTableDumpFile;
        zompIncludePaths = !zompIncludePaths;
        dllPaths = !dllPaths;
      }
    with
      | Arg.Bad msg
      | Arg.Help msg ->
        InvalidArguments msg
end

let reportCommandLineArgumentError message =
  printf "error: %s\n" message;
  printf "zompc -c fileName.zomp\n";
  printf "to compile fileName.zomp into fileName.ll\n"

let getBasename filename =
  let zompFileRE = Str.regexp "\\(.+\\)\\.zomp" in
  if Str.string_match zompFileRE filename 0 then
    Some (Str.matched_group 1 filename)
  else
    None

type compilation_result =
  | Compilation_succeeded of Bindings.t
  | Compiler_did_not_return_result
  | Compilation_failed_with_error of string
  | Failed_to_init_vm

let compilation_result_to_int = function
  | Compilation_succeeded _ -> 0
  | Compilation_failed_with_error _ -> 1
  | Compiler_did_not_return_result -> 2
  | Failed_to_init_vm -> 4

let preludeTime = ref 0.0
let mainFileTime = ref 0.0

let section = Statistics.createSection "zompc"

let () =
  Statistics.createFloatCounter section "compile prelude (s)" 3 (Ref.getter preludeTime);
  Statistics.createFloatCounter section "compile main file (s)" 3 (Ref.getter mainFileTime);
  ()

let compile initEnv fileName inStream outStream =
  let preludeDir = Filename.dirname Sys.executable_name ^ "/../../../source" in
  let input =
    collectTimingInfo "reading prelude file content" (fun () -> Common.readChannel inStream)
  in

  let reportDiagnostics diag =
    eprintf "%s\n" $ Serror.toString diag;
    flush stderr
  in

  if not( Zompvm.init() ) then begin
    Failed_to_init_vm
  end else begin
    Zompvm.setVerifyCode false;
    let env = Expander.createEnv Genllvm.defaultBindings in
    initEnv env;
    let exitCode =
      let preludeResult =
        addTiming preludeTime $ fun () -> Compileutils.loadPrelude env preludeDir
      in
      List.iter reportDiagnostics preludeResult.Result.diagnostics;
      if Result.failed preludeResult then
        Compilation_failed_with_error "failed to compile prelude"
      else begin
        addTiming mainFileTime $ fun () ->
          let { Result.flag; diagnostics; _ } =
            Compileutils.compileFromStream env ~source:input ~fileName
          in
          List.iter reportDiagnostics diagnostics;
          if flag = Result.Success then
            Compilation_succeeded (Expander.bindings env)
          else
            Compiler_did_not_return_result
      end
    in
    Zompvm.shutdown();
    exitCode
  end

let () =
  let startTime = Sys.time() in

  (** causes crashes in 64-bit OCaml 3.12 *)
  if Sys.word_size = 32 then
    Printexc.record_backtrace true;

  let open Options in
  let options =
    match Options.extract Sys.argv with
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
      Zompvm.printTimingStats();
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
        printf "%s: Expansion step %s:\n%s\n" locString s (Common.indent ~count:4 $ Ast2.toString e);
        flush stdout;
      end
    in
    Expander.setTraceMacroExpansion (Some trace);
    Zompvm.traceMacroExpansionOn := true;
  end;
  if options.traceBaseForms then begin
    let reportInfo diagnostic =
      eprintf "%s\n" (Serror.diagnosticsToString Basics.DiagnosticKind.Info diagnostic)
    in
    let trace form =
      reportInfo $ Serror.fromMsg (Some (Lang.toplevelFormLocation form)) (Lang.toplevelFormDeclToString form)
    in
    Expander.setTraceToplevelForm (Some trace);
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

  let handleLLVMCode code =
    let ir = Zompvm.codeToString code in
    if options.traceLlvmCode then begin
      printf "llvm code:\n%s\n---\n" ir;
      flush stdout;
    end;
    output_string outStream ir
  in
  (* TODO: emit Llvm Ir from Zompvm after compilation instead of dumping text here! *)
  Zompvm.registerCodeHandler handleLLVMCode;

  let initEnv env =
    let addDllPath where dir = Expander.addDllPath env dir where in
    List.iter (addDllPath `Back) Expander.recommendedDllPath;
    List.iter (addDllPath `Front) options.dllPaths;

    let addIncludePath where dir = Expander.addIncludePath env dir where in
    List.iter (addIncludePath `Back) Expander.recommendedIncludePath;
    List.iter (addIncludePath `Front) options.zompIncludePaths;
  in

  let exitCode =
    try
      compile initEnv inputFileName inStream outStream
    with _ as e ->
      Compilation_failed_with_error
        (sprintf "failed to compile due to unknown exception: %s\n%s\n"
           (Printexc.to_string e)
            (** returns backtrace of last thrown exception *)
           (Printexc.get_backtrace()))
  in

  close_in inStream;
  close_out outStream;

  let failWithErrorMessage msg =
    eprintf "%s:0: error: %s\n" inputFileName msg;
    Sys.remove outputFileName;
  in

  begin match exitCode with
    | Compilation_succeeded globalBindings ->
      begin match options.symbolTableDumpFile with
        | Some absoluteFileName ->
          if not (Compileutils.writeSymbols globalBindings absoluteFileName) then
            eprintf "error: could not write symbols to '%s'\n" absoluteFileName;
        | None -> ();
      end
    | Failed_to_init_vm ->
      failWithErrorMessage "could not init VM"
    | Compiler_did_not_return_result ->
      failWithErrorMessage "compiler did not return a result"
    | Compilation_failed_with_error msg ->
      failWithErrorMessage msg
  end;

  let endTime = Sys.time() in
  Statistics.createFloatCounter section "compilation time (s)" 3 (fun () -> endTime -. startTime);

  exit (compilation_result_to_int exitCode)

