
open Ast2
open Printf
open Lang
open Common
open Bindings

let version = "0.?"
let zompShellDummyFileName = "zompsh"

let reportError msg =
  eprintf "error: %s\n" msg;
  flush stderr

let report error =
  eprintf "%s\n" (Serror.toString error);
  flush stderr

let message msg =
  printf "%s\n" msg;
  flush stdout

let toplevelCommandChar = '!'
let toplevelCommandString = String.make 1 toplevelCommandChar

let defaultPrompt = ref "  # "
and continuedPrompt = ref "..# "

let printLLVMCode = ref false
and printAst = ref false
and printDeclarations = ref false
and printForms = ref false
and showStatsAtExit = ref false
and showTimingStatsAtExit = ref false
and statsAtExitFile = ref ""
and traceMacroExpansion = ref false
and hadErrors = ref false

let parseFunc = ref Parseutils.parseIExpr
let notifyTimeThreshold = ref 0.5

let runFunction bindings funcname =
  printf "\n"; flush stdout;
  match Bindings.lookup bindings funcname with
    | FuncSymbol func ->
      begin
        match func.rettype with
          | `Void ->
            Machine.zompRunFunction funcname;
          | `Int32 ->
            let retval = Machine.zompRunFunctionInt funcname in
            printf " => %d\n" retval
          | `Bool ->
            let retval = Machine.zompRunFunctionBool funcname in
            printf " => %b\n" retval
          | `Pointer `Char ->
            let retval = Machine.zompRunFunctionString funcname in
            printf " => %s\n" retval
          | otherRetType ->
            printf "cannot call a function which returns %s\n" (Typesystems.Zomp.typeName otherRetType)
      end
    | _ ->
      eprintf "cannot run %s because no such function was found\n" funcname

module Symbol_stats : sig
  val update : Bindings.t -> unit
  val sourceFiles : unit -> string list
end = struct
  let allSourceFiles = ref []
  let sourceFiles () = !allSourceFiles

  let section =
    Statistics.createSection "zompsh symbols"

  let intCounter name =
    let stat postfix =
      let r = ref 0 in
      Statistics.createIntCounter section (name ^ postfix) 0 (Ref.getter r);
      r
    in
    let total = stat ""
    and valid = stat " w/ valid location"
    and noloc = stat " w/o location"
    and fake = stat " fake location"
    and builtin = stat " built-in" in
    total, valid, noloc, fake, builtin

  let symbolStat = intCounter "symbols"
  let varStat = intCounter "vars"
  let functionDefStat = intCounter "functions"
  let functionDeclStat = intCounter "function declarations"
  let macroStat = intCounter "macros"
  let typeStat = intCounter "types"

  let update bindings =
    let fileNames = ref [] in
    let count loc (total, validLoc, noLoc, fakeLoc, builtinLoc) =
      incr total;
      begin match loc with
        | None ->
          incr noLoc;
        | Some loc when loc = Basics.fakeLocation ->
          incr fakeLoc
        | Some loc when loc = Basics.builtinLocation ->
          incr builtinLoc
        | Some loc ->
          incr validLoc;
          fileNames := loc.Basics.fileName :: !fileNames
      end;
    in

    let statForSymbolInfo info =
      match Bindings.symbol info with
        | Bindings.VarSymbol _ ->
          Some varStat
        | Bindings.FuncSymbol { impl = Some _ } ->
          Some functionDefStat
        | Bindings.FuncSymbol { impl = None } ->
          Some functionDeclStat
        | Bindings.MacroSymbol _ ->
          Some macroStat
        | Bindings.LabelSymbol _ ->
          None
        | Bindings.TypedefSymbol _ ->
          Some typeStat
        | Bindings.UndefinedSymbol ->
          None
    in

    let countSymbol (name, info) =
      let location = Bindings.location info in
      begin match statForSymbolInfo info with
        | Some stat -> count location stat
        | None -> ()
      end;
      count location symbolStat;
    in
    Bindings.iterInfo countSymbol bindings;

    let module S = Set.Make(String) in
    let files = List.fold_right S.add !fileNames S.empty in
    allSourceFiles := S.fold (fun file list -> file :: list) files []
end

let currentFile = ref "zompsh"
(** Added to line of every parsed expression *)
let firstLineDelta = ref 0

module Commands : sig
  val handleCommand : string -> Bindings.bindings -> unit
end = struct
  module StringMap = Map.Make(String)

  type commandFunc = string list -> Bindings.bindings -> unit

  let arglistToString argL = Common.combine " " argL

  let makeNoArgCommand f =
    fun argL bindings ->
      match argL with
        | [] ->
          f bindings
        | (_ :string list) ->
          eprintf "error: expected no arguments instead of %s\n" (arglistToString argL)

  let makeSingleArgCommand f =
    fun argL bindings ->
      match argL with
        | [arg] ->
          f arg bindings
        | _ ->
          eprintf "error: expected single argument instead of %s\n" (arglistToString argL)

  let makeOptionalArgCommand f =
    fun argL bindings ->
      match argL with
        | [] -> f bindings None
        | [arg] -> f bindings (Some arg)
        | _ ->
          eprintf "error: expected one or zero arguments instead of %s" (arglistToString argL)

  let makeToggleCommandFromGetSet name getF setF =
    let change ison =
      printf "%s %s\n" name (if ison then "enabled" else "disabled");
      flush stdout;
      setF ison
    in
    fun args (_:bindings) ->
      match args with
        | ["on"] | ["yes"] | ["true"] -> change true
        | ["off"] | ["no"] | ["false"] -> change false
        | [] -> change (not (getF()))
        | _ ->
          eprintf "error: expected on/off/yes/no/true/false or no arguments to toggle instead of %s\n"
            (arglistToString args)

  let makeToggleCommandForRef refvar name =
    makeToggleCommandFromGetSet
      name
      (fun () -> !refvar)
      (fun v -> refvar := v)

  let changePromptCommand args _ =
    print_newline();
    let removeQuotes quoteChar str =
      match dequoteString quoteChar str with | `NotQuoted str | `Quoted str -> str
    in
    match List.map (removeQuotes '\'' ++ removeQuotes '"') args with
      | [] ->
        defaultPrompt := "";
        continuedPrompt := "";
      | [newPrompt] ->
        defaultPrompt := "  " ^ newPrompt ^ " ";
        continuedPrompt := ".." ^ newPrompt ^ " ";
      | [newDefault; newContinued] ->
        defaultPrompt := newDefault;
        continuedPrompt := newContinued;
      | args ->
        eprintf "expected 0-2 arguments instead of %s\n" (arglistToString args)

  let exitCommand = makeNoArgCommand
    (fun bindings ->
      Symbol_stats.update bindings;

      printf "Exiting.\n";
      exit (if !hadErrors then 1 else 0))

  let toggleAstCommand = makeToggleCommandForRef printAst "Printing s-expressions"
  let toggleLLVMCommand = makeToggleCommandForRef printLLVMCode "Printing LLVM code"
  let togglePrintDeclarationsCommand =
    makeToggleCommandForRef printDeclarations "Printing declarations"
  let togglePrintFormsCommand =
    makeToggleCommandForRef printForms "Print translated forms"

  let toggleShowStatsAtExitCommand =
    makeToggleCommandForRef showStatsAtExit "Show stats at exit"
  let toggleShowTimingStatsAtExitCommand =
    makeToggleCommandForRef showTimingStatsAtExit "Shot timing stats at exit"

  let printStatsCommand =
    makeOptionalArgCommand (fun bindings arg ->
      Symbol_stats.update bindings;
      match arg with
      | None ->
        flush stdout;
        Stats.statsPrintReport 0
      | Some fileName ->
        if (not (Stats.statsPrintReportToFile fileName 0)) then
          eprintf "error: could not print statistics to file %s\n" fileName)

  let printSourceFilesCommand =
    makeNoArgCommand (fun bindings ->
      Symbol_stats.update bindings;
      printf "Source files of all symbols:\n";
      List.iter (printf "  file %s\n") (Symbol_stats.sourceFiles()))

  let toggleTraceMacroExpansionCommand =
    makeToggleCommandForRef traceMacroExpansion "Trace macro expansion"

  let toggleParseFunc args _ =
    let confirm syntax = printf "Changed syntax to %s\n" syntax in
    match args with
      | ["indent"] -> parseFunc := Parseutils.parseIExpr; confirm "indent"
      | _ -> printf "error: invalid option. Use sexpr or indent\n"

  let toggleVerifyCommand =
    makeToggleCommandFromGetSet
      "Verifying LLVM code"
      Zompvm.zompDoesVerifyCode
      (fun b -> Zompvm.zompVerifyCode b)
  let toggleOptimizeFunctionCommand =
    makeToggleCommandFromGetSet
      "Optimize LLVM code"
      Zompvm.zompOptimizeFunction
      (fun b -> Zompvm.zompSetOptimizeFunction b)

  let setNotifyTimeThresholdCommand = makeSingleArgCommand
    (fun timeStr _ ->
      try
        let newTime = float_of_string timeStr in
        if newTime >= 0.0 then
          notifyTimeThreshold := newTime
        else
          printf "error: given time must be at least zero\n"
      with Failure "float_of_string" ->
        printf "error: could not parse '%s' as float\n" timeStr)

  let printBindingsCommand args (bindings :bindings) =
    let regexps = List.map
      (fun restr -> Str.regexp (sprintf ".*%s.*" (String.lowercase restr)))
      args
    in
    let printSymbol (name, symbol)  =
      if List.for_all (fun re -> Str.string_match re (String.lowercase name) 0) regexps then
        match symbol with
          | VarSymbol var ->
            printf "var %s %s\n" (Lang.typeName var.typ) var.vname
          | FuncSymbol f ->
            let funcSigString f =
              let arg2string (name, typ) = Lang.typeName typ ^ " " ^ name in
              let args = List.map arg2string f.fargs in
              let argString = combine ", " args in
              let typeParams = if f.fparametric then "!T" else "" in
              sprintf "func %s %s%s(%s)\n"
                (Lang.typeName f.rettype)
                f.fname
                typeParams
                argString
            in
            printf "%s" (funcSigString f)
          | MacroSymbol m ->
            printf "macro %s %s\n" m.mname m.mdocstring
          | TypedefSymbol t ->
            printf "type %s = %s\n" name (typeDescr t)
          | LabelSymbol { lname = name; } ->
            printf "label %s\n" name
          | UndefinedSymbol ->
            printf "undefined %s\n" name
    in
    print_newline();
    Bindings.iter printSymbol bindings;
    print_newline()

  let setSourceLocation args (_ :bindings) =
    match args with
      | [fileName; lineStr] ->
        begin try
          let line = int_of_string lineStr in
          printf "source location set to %s %d\n" fileName line;
          currentFile := fileName;
          firstLineDelta := line - 1;
        with (Failure _) ->
          eprintf "error: could not parse line."
        end
      | _ ->
        reportError "expected arguments fileName and line"

  let runMainCommand = makeSingleArgCommand
    (fun funcname bindings ->
      runFunction bindings funcname)



  let loadCodeCommand args bindings =
    let matches re string = Str.string_match (Str.regexp re) string 0 in
    List.iter
      (fun name ->
        if matches ".*\\.ll" name then
          Zompvm.loadLLVMFile name
        else
          printf "error: unsupported file extension\n" )
      args

  let connectToRemoteVMCommand = makeSingleArgCommand
    (fun uri (_:bindings) ->
      if Machine.zompConnectToRemoteVM uri then
        printf "Connected to remote VM at %s" uri
      else
        eprintf "error: failed to connect to remote VM at %s" uri)

  let disconnectRemoteVMCommand = makeNoArgCommand
    (fun (_:bindings) -> Machine.zompDisconnectRemoteVM())

  let requestUriFromRemoteVMCommand = makeSingleArgCommand
    (fun uri (_:bindings) -> Machine.zompSendToRemoteVM uri)


  let listCommands commands = makeNoArgCommand
    (fun _ ->
      printf "%c to abort multi-line (continued input)\n" toplevelCommandChar;
      let maxCommandLength =
        List.fold_left (fun oldMax (name, _, _, _) -> max oldMax (String.length name)) 5 commands
      in
      let printCommand (name, aliases, _, doc) =
        let aliasString = if List.length aliases > 0 then " (also " ^ combine ", " aliases ^ ")" else "" in
        printf "!%-*s - %s%s\n" (maxCommandLength+1) name doc aliasString
      in
      List.iter printCommand commands)

  let writeSymbolsCommand = makeSingleArgCommand
    (fun fileName bindings ->
      if not (Compileutils.writeSymbols bindings fileName) then
        reportError (sprintf "could not write symbols to file '%s'" fileName))

  let printSymbolsCommand = makeNoArgCommand
    (fun bindings ->
      Compileutils.writeSymbolsToStream bindings stdout)

  let optimizeCommand = makeNoArgCommand
    (fun _ ->
      Machine.zompOptimizeFunctions();
      printf "Ran optimizations on all functions\n";
      flush stdout)

  let writeLLVMCodeToFileCommand = makeSingleArgCommand
    (fun fileName _ ->
      Machine.zompWriteLLVMCodeToFile fileName;
      printf "Wrote LLVM code to file %s\n" fileName;
      flush stdout)

  let echoCommand args (_:bindings) =
    List.iter (printf "%s ") args;
    print_newline();
    flush stdout

  let printVersionInfoCommand = makeNoArgCommand
    (fun _ ->
      printf "Version %s, build %s\n" version (Zompvm.zompBuildInfo());
      flush stdout)

  let commands =
    let rec internalCommands = [
      "bindings", ["b"], printBindingsCommand, "Print a list of defined symbols";
      "echo", [], echoCommand, "Echo all given parameters";
      "exit", ["x"; "q"], exitCommand, "Exit";
      "help", ["h"], printHelpCommand, "List all toplevel commands";
      "llvm", [], toggleLLVMCommand, "Toggle printing of llvm code";
      "load", [], loadCodeCommand, "Load code. Supports .ll files";
      "traceMacros", [], toggleTraceMacroExpansionCommand, "Toggle tracing of macro expansion";
      "optimize", [], optimizeCommand, "Optimize all functions";
      "printAst", [], toggleAstCommand, "Toggle printing of parsed s-expressions";
      "printBaseLang", [], togglePrintFormsCommand, "Toggle printing translated base lang forms";
      "printDecl", [], togglePrintDeclarationsCommand, "Toggle printing declarations";
      "printllvm", ["pl"], (fun _ _ -> Machine.zompPrintModuleCode()), "Print LLVM code in module";
      "printStats", [], printStatsCommand, "Print statistics";
      "printSourceFiles", [], printSourceFilesCommand, "Print all source files of symbols";
      "prompt", [], changePromptCommand, "Set prompt";
      "run", [], runMainCommand, "Run a function of type void(void), default main";
      "setNotifyTimeThresholdCommand", [], setNotifyTimeThresholdCommand, "Set minimum compilation time to print timing information";
      "setOptimizeFunctions", [], toggleOptimizeFunctionCommand, "Optimize functions on definition";
      "setSourceLocation", [], setSourceLocation, "Set source location of next line entered";
      "showStatsAtExit", [], toggleShowStatsAtExitCommand, "Show statistics at exit";
      "showTimingStatsAtExit", [], toggleShowTimingStatsAtExitCommand, "Show timing statistics at exit";
      "syntax", [], toggleParseFunc, "Choose a syntax";
      "verify", ["v"], toggleVerifyCommand, "Verify generated llvm code";
      "version", [], printVersionInfoCommand, "Print version/build info";
      "writeSymbols", [], writeSymbolsCommand, "Write all symbols to given file for emacs eldoc-mode";
      "printSymbols", [], printSymbolsCommand, "Like writeSymbols but prints to stdout";
      "writellvm", [], writeLLVMCodeToFileCommand, "Write LLVM code to file";

      "connect", [], connectToRemoteVMCommand, "Connect to remote ZompVM server";
      "disconnect", [], disconnectRemoteVMCommand, "Disconnect from remote ZompVM server";
      "requestUri", [], requestUriFromRemoteVMCommand, "Send request to URI to remote ZompVM server";
    ]
    and printHelpCommand ignored1 ignored2 =
      printf "Enter Zomp code to get it evaluated. Directly run code using std:base:run\n\n";
      listCommands internalCommands ignored1 ignored2
    in
    let addCommands map (mainName, aliases, func, doc) =
      let newMap = StringMap.add mainName (func, doc) map in
      let docRef = sprintf "see %s" mainName in
      List.fold_left (fun map name -> StringMap.add name (func, docRef) map) newMap aliases
    in
    List.fold_left addCommands StringMap.empty internalCommands

  let handleCommand commandLine bindings =
    let commandRE = Str.regexp (toplevelCommandString ^ "\\([a-zA-Z0-9]+\\)\\(.*\\)") in
    if Str.string_match commandRE commandLine 0 then
      let commandName = Str.matched_group 1 commandLine
      and argString = Str.matched_group 2 commandLine
      in
      let argList = Str.split (Str.regexp "[\t ]+") argString in
      try
        let (func, _) = StringMap.find commandName commands in
        func argList bindings
      with
        | Not_found ->
          reportError (sprintf "could not find command '%s'" commandName)
    else
      reportError (sprintf "'%s' is not a command string" commandLine)
end

type 'a mayfail =
  | Result of 'a
  | Error of string list

let readExpr bindings =
  let silentPrefix = "!silent" in

  (** Reads one line of source code. Will interpret and run shell commands *)
  let rec readSource previousLinesRev =
    let handleCommandAndContinue command =
      Commands.handleCommand command bindings;
      if command =~ "!setSourceLocation " then begin
        if List.exists (fun str -> String.length str > 0) previousLinesRev then begin
          reportError "!setSourceLocation must not be used after entering code"
        end;
        readSource []
      end else
        readSource ("" :: previousLinesRev)
    in
    if List.length previousLinesRev = 0 || not (List.exists (fun s -> String.length s > 0) previousLinesRev) then
      printf "%s" !defaultPrompt
    else
      printf "%s" !continuedPrompt;
    flush stdout;

    let line = read_line() in

    if line |> beginsWith silentPrefix then begin
      let command = removeBeginning line (String.length silentPrefix + 1) in
      handleCommandAndContinue command

    end else if line = toplevelCommandString then begin
      printf "Aborted input, cleared \"%s\"\n"
        (previousLinesRev |> List.rev |> Common.combine "\n");
      readSource ("" :: List.map (fun _ -> "") previousLinesRev)

    end else if nthChar 0 line = Some toplevelCommandChar then begin
      handleCommandAndContinue line
    end else begin
      line :: previousLinesRev
    end
  in

  let fixSourceLocation expr =
    let rec fixit expr =
      let args = List.map fixit expr.args in
      let location = {
        Basics.fileName = !currentFile;
        line = Ast2.lineNumber expr + !firstLineDelta;
        column = match expr.location with
          | Some { Basics.column } -> column
          | _ -> None
      }
      in
      { expr with args = args; location = Some location }
    in
    fixit expr
  in

  let parse source =
    let source = source ^ "\n" in
    match Parseutils.parseIExprs ~fileName:zompShellDummyFileName source with
      | Parseutils.Exprs exprs ->
        let exprs = (List.map fixSourceLocation exprs) in
        Result exprs
      | Parseutils.Error e -> Error [Serror.toString e]
  in

  let rec read wsLines previousLinesRev =
    if wsLines >= 2 then
      let source = Common.combine "\n" $ List.rev previousLinesRev in
      let result = parse source in
      firstLineDelta := !firstLineDelta + List.length previousLinesRev;
      result
    else begin
      let linesRev = readSource previousLinesRev in
      begin match linesRev with
        | line :: _ when isWhitespaceString line ->
          read (wsLines + 1) linesRev
        | _ ->
          read 0 linesRev
      end
    end
  in
  read 0 []

(** Parsing function which can be called from Zomp/native code. *)
let parseNativeAst ~fileName str =
  let expr =
    match Parseutils.parseIExprs ~fileName str with
      | Parseutils.Exprs [expr] -> expr
      | Parseutils.Exprs exprs -> Ast2.seqExpr exprs
      | Parseutils.Error error ->
          let msg = Serror.toString error in
          Ast2.juxExpr [Ast2.idExpr "error"; Ast2.idExpr msg]
  in
  Zompvm.NativeAst.buildNativeAst expr

let onToplevelForm form =
  if !printDeclarations then begin
    let reportInfo diagnostic =
      eprintf "%s\n" (Serror.diagnosticsToString Basics.DiagnosticKind.Info diagnostic)
    in
    reportInfo $ Serror.fromMsg (Some (Lang.toplevelFormLocation form)) (Lang.toplevelFormDeclToString form)
  end;

  if !printForms then begin
    printf "%s\n" $ toplevelFormToString form;
  end;
  flush stdout

let onLlvmCode llvmCode =
  if !printLLVMCode then begin
    printf "LLVM code:\n%s\n" llvmCode;
    flush stdout;
  end

(** Produce and run an immediate function from the given code. *)
let translateRun env expr =
  let immediateFuncName = "toplevel:immediate" in

  match expr with
    | { args = [code] } -> begin
        let exprInFunc =
          Ast2.expr "func" [
            idExpr "void";
            callExpr [idExpr immediateFuncName];
            opseqExpr [
              code;
              Ast2.expr macroReturn []]]
        in
        try
          let { Result.flag; diagnostics; results = _ }, _ =
            Compileutils.compileExpr env exprInFunc
          in
          List.iter report diagnostics;
          runFunction (Expander.bindings env) immediateFuncName;
        with
          | Expander.IllegalExpression (_, errors) ->
            List.iter (Expander.emitError env) errors;
          | exn ->
            Expander.emitError env $ Serror.fromExpr expr (Printexc.to_string exn)
      end
    | _ ->
      Expander.emitError env $ Serror.fromExpr expr (sprintf "expected %s expr" expr.id)

let rec step env parseState =
  let bindings = Expander.bindings env in
  match parseState with
    | Error errors ->
      printf "Parser errors:\n";
      List.iter (printf "%s\n") errors;
      flush stdout;
      step env (readExpr bindings)

    | Result [] ->
      step env (readExpr bindings)

    | Result (expr :: remExprs) ->
      if !printAst then begin
        let asString = Ast2.toString expr in
        printf " => %s\n" asString;
      end;

      Expander.setTraceMacroExpansion
        (if !traceMacroExpansion then
            let trace s e =
              printf "Expansion step %s:\n%s\n" s (Common.indent ~count:4 $ Ast2.toString e)
            in
            Some trace
         else
            None);

      let (), time = recordTiming (fun () ->
        let { Result.flag; diagnostics; results = _ }, _ =
          Compileutils.compileExpr env expr
        in
        List.iter report diagnostics;
        if flag = Result.Fail then begin
          hadErrors := true;
        end)
      in
      if (time > !notifyTimeThreshold) then
        printf "Compiling expression took %fs\n" time;
      step env (Result remExprs)

let init() =
  Zompvm.setIsInteractive true;
  Callback.register "parse" parseNativeAst;
  if not (Machine.zompInit()) then begin
    eprintf "Could not initialize ZompVM\n";
    exit(-1);
  end;
  at_exit Machine.zompShutdown

let loadPrelude env () =
  let defaultMainSrc =
    "std:base:func int main():\n  printString\"error: no main method defined\\n\"\n  std:base:ret 1\nend\n"
  in
  let preludeDir = Filename.dirname Sys.executable_name ^ "/../../../source" in
  let r = Compileutils.loadPrelude env ~appendSource:defaultMainSrc preludeDir in
  begin match r with
    | { Result.flag = Result.Fail; diagnostics } ->
      List.iter report diagnostics
    | _ -> ()
  end;
  Result.succeeded r

let handleOptions args =
  let onAnonArg str =
    raise (Arg.Bad (sprintf "%s: anonymous arguments not supported" str))
  in
  try
    Arg.parse_argv args
      ["--stats", Arg.Set_string statsAtExitFile, "print statistics on exit"]
      onAnonArg
      "zompsh\n"
  with
    | Arg.Bad msg
    | Arg.Help msg ->
      reportError msg;
      exit 1

let () =
  handleOptions Sys.argv;

  at_exit (fun () ->
    if !showTimingStatsAtExit then begin
      Profiling.printTimings();
      flush stdout;
      Zompvm.zompPrintTimingStats();
    end;

    if !showStatsAtExit then
      Stats.statsPrintReport 0;

    match !statsAtExitFile with
      | "" -> ()
      | file ->
        if Stats.statsPrintReportToFile (Common.absolutePath file) 0 = false then
          reportError $ sprintf "could not write stats to file '%s'" file);

  Zompvm.zompVerifyCode false;

  message (sprintf "Welcome to Zomp shell, version %s%s"
             version
             (if Zompvm.zompIsDebugBuild() then ", Debug build" else ""));

  init();

  Expander.setEmitbackendCode onLlvmCode;
  Expander.setTraceToplevelForm (Some onToplevelForm);

  let env = Expander.createEnv Genllvm.defaultBindings in

  let addDllPath path where = Expander.addDllPath env path where in
  addDllPath "." `Back;
  addDllPath "./libs" `Back;
  addDllPath "./tools/external/lib" `Back;

  Expander.addIncludePath env "." `Front;

  let preludeOk, preludeLoadTime = recordTiming (loadPrelude env) in
  printf "Loading prelude took %.2fs\n" preludeLoadTime;
  let section = Statistics.createSection "zompsh" in
  Statistics.createFloatCounter section "prelude load time (s)" 3 (fun () -> preludeLoadTime);

  if preludeOk then begin
    message (sprintf "%cx - exit, %chelp - help.\n" toplevelCommandChar toplevelCommandChar);
    Expander.addTranslateFunction "std:base:run" "statement..." translateRun;
    let `NoReturn = step env (Result []) in ()
  end else begin
    reportError "failed to load prelude";
    exit(-2);
  end


