
open Ast2
open Printf
open Lang
open Common
open Bindings

let version = "0.?"
let zompShellDummyFileName = "zompsh"

let reportError msg =
  eprintf "error: %s\n" msg
let report error =
  eprintf "%s\n" (Serror.toString error)

let toplevelCommandChar = '!'
let toplevelCommandString = String.make 1 toplevelCommandChar

let defaultPrompt = ref "  # "
and continuedPrompt = ref "..# "

let printLLVMCode = ref false
and printAst = ref false
and printDeclarations = ref true
and llvmEvaluationOn = ref true
and printForms = ref false
and showStatsAtExit = ref false
and showTimingStatsAtExit = ref false
and traceMacroExpansion = ref false

module StringMap = Map.Make(String)

exception AbortExpr

let parseFunc = ref Parseutils.parseIExpr

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

let notifyTimeThreshold = ref 0.5

module Utils =
struct
  let beginsWith word line =
    let wordLength = String.length word
    and lineLength = String.length line
    in
    lineLength >= wordLength &&
      (String.sub line 0 (String.length word)) = word

  let removeBeginning text count =
    let textLength = String.length text in
    let count = min textLength count in
    String.sub text count (textLength - count)

  let nthChar num string =
    if String.length string > num then
      Some string.[num]
    else
      None

  let isWhitespaceString str =
    let strLength = String.length str in
    let rec worker n =
      if n >= strLength then true
      else
        let c = str.[n] in
        (c == ' ' || c == '\n') && worker (n+1)
    in
    worker 0

  let matchAnyRegexp patterns =
    match patterns with
      | [] -> Str.regexp ".*"
      | _ ->
        let containsPatterns = List.map (String.lowercase ++ sprintf ".*%s.*") patterns in
        Str.regexp ( "\\(" ^ combine "\\|" containsPatterns ^ "\\)" )

  let recordTiming f =
    let startTime = Sys.time() in
    let result = f() in
    let endTime = Sys.time() in
    result, (endTime -. startTime)
end

open Utils

let currentLocation : Basics.location option ref = ref None

module Commands : sig
  val handleCommand : string -> Bindings.bindings -> unit
end = struct
  type commandFunc = string list -> Bindings.bindings -> unit

  let makeNoArgCommand f =
    fun argL bindings ->
      match argL with
        | [] ->
          f bindings
        | (_ :string list) ->
          eprintf "expected 0 arguments\n";
          flush stderr

  let makeSingleArgCommand f =
    fun argL bindings ->
      match argL with
        | [arg] ->
          f arg bindings
        | _ ->
          eprintf "expected single argument"

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
          reportError "expected on/off/yes/no/true/false or no arguments to toggle"

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
        printf "expected 0-2 arguments instead of %d\n" (List.length args)

  let exitCommand = makeNoArgCommand
    (fun _ ->
      printf "Exiting.\n";
      exit 0)

  let toggleAstCommand = makeToggleCommandForRef printAst "Printing s-expressions"
  let toggleLLVMCommand = makeToggleCommandForRef printLLVMCode "Printing LLVM code"
  let togglePrintDeclarationsCommand =
    makeToggleCommandForRef printDeclarations "Printing declarations"
  let toggleEvalCommand =
    makeToggleCommandForRef llvmEvaluationOn "Evaluating LLVM code"
  let togglePrintFormsCommand =
    makeToggleCommandForRef printForms "Print translated forms"

  let toggleShowStatsAtExitCommand =
    makeToggleCommandForRef showStatsAtExit "Show stats at exit"
  let toggleShowTimingStatsAtExitCommand =
    makeToggleCommandForRef showTimingStatsAtExit "Shot timing stats at exit"
  let printStatsCommand =
    makeNoArgCommand (fun _ -> Stats.statsPrintReport 0)

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
          let column = Some 0 in
          printf "setSourceLocation %s %d\n" fileName line;
          currentLocation := Some { Basics.fileName; line; column }
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
      "eval", [], toggleEvalCommand, "Toggle evaluation of llvm code";
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
  let rec readSourceLine previousLines =
    if String.length previousLines = 0 then
      printf "%s" !defaultPrompt
    else
      printf "%s" !continuedPrompt;
    flush stdout;

    let line = read_line() in

    if line |> beginsWith silentPrefix then begin
      let command = removeBeginning line (String.length silentPrefix + 1) in
      Commands.handleCommand command bindings;
      readSourceLine previousLines

    end else if line = toplevelCommandString then begin
      printf "Aborted input, cleared \"%s\"\n" previousLines;
      readSourceLine ""

    end else if nthChar 0 line = Some toplevelCommandChar then begin
      Commands.handleCommand line bindings;
      readSourceLine previousLines
    end else begin
      line, previousLines
    end
  in

  let fixSourceLocation expr =
    let rec fixit loc expr =
      let args = List.map (fixit loc) expr.args in
      let location = {
        Basics.fileName = loc.Basics.fileName;
        line = Ast2.lineNumber expr + loc.Basics.line;
        column = match expr.location with
          | Some { Basics.column } -> column
          | _ -> None
      }
      in
      { expr with args = args; location = Some location }
    in
    match !currentLocation with
      | Some loc -> fixit loc expr
      | None -> expr
  in

  let parse source =
    let source = source ^ "\n" in
    match Parseutils.parseIExprs ~fileName:zompShellDummyFileName source with
      | Parseutils.Exprs exprs ->
        let exprs = (List.map fixSourceLocation exprs) in
        (match exprs with
          | e :: _ ->
            printf "parsed at %s:%d\n" (Ast2.fileName e) (Ast2.lineNumber e)
          | [] -> ());
        Result exprs
      | Parseutils.Error e -> Error [Serror.toString e]
  in

  let rec read wsLines previousLines =
    if wsLines >= 2 then
      parse previousLines
    else begin
      let line, previousLines = readSourceLine previousLines in
      if isWhitespaceString line then
        read (wsLines + 1) previousLines
      else
        read 0 (previousLines ^ "\n" ^ line)
    end
  in
  read 0 ""

module CompilerInstructions =
struct
  open Expander
  open Ast2

  let translateLinkCLib dllPath env = function
    | { args = [{id = fileName; args = []}] } as expr ->
        begin
          let fileName = Common.removeQuotes fileName in
          let dllExtensions = ["dylib"; "so"; "dll"] in
          let matches re string = Str.string_match (Str.regexp re) string 0 in
          let dllPattern = sprintf ".*\\.\\(%s\\)" (Common.combine "\\|" dllExtensions) in
          if not (matches dllPattern fileName) then
            Expander.errorFromStringDeprecated
              (sprintf "%s has invalid extension for a dll. Supported: %s"
                 fileName (Common.combine ", " dllExtensions))
          else
            match findFileIn fileName !dllPath with
              | Some fullName ->
                let handle = Zompvm.zompLoadLib fullName in
                if handle = 0 then
                  Expander.errorFromExpr expr
                    (sprintf "could not load C library '%s'\n" fullName)
                else
                  Expander.tlReturnNoExprs env
              | None ->
                Expander.errorFromExpr expr
                  (Common.combine "\n  "
                     (sprintf "could not load C library '%s'," fileName
                      :: sprintf "pwd = %s" (Sys.getcwd())
                      :: List.map (sprintf "zomp-include-dir %s") !dllPath))
        end
    | invalidExpr ->
        Expander.errorFromStringDeprecated
          (sprintf "expecting '%s fileName" invalidExpr.Ast2.id)
end

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

let onSuccess bindings newBindings simpleforms llvmCode =
  if !printLLVMCode then begin
    printf "LLVM code:\n%s\n" llvmCode;
    flush stdout;
  end;

  if !llvmEvaluationOn then
    Zompvm.evalLLVMCode bindings simpleforms llvmCode;

  if !printDeclarations then begin
    List.iter (fun form ->
                 let text = Lang.toplevelFormDeclToString form in
                 printf "%s\n" text)
      simpleforms;
  end;

  if !printForms then begin
    List.iter (fun form -> printf "%s\n" (toplevelFormToString form)) simpleforms;
  end;
  flush stdout

(** Produce and run an immediate function from the given code. *)
let translateRun env expr =
  let immediateFuncName = "toplevel:immediate" in

  match expr with
    | { args = [code] } -> begin
        let exprInFunc =
          Ast2.expr "func" [
            idExpr "void";
            callExpr [
              idExpr immediateFuncName;
            ];
            opseqExpr [
              code;
              Ast2.expr macroReturn []]
          ]
        in
        try
          let newBindings, simpleforms, llvmCode =
            Compileutils.compileExpr
              Compileutils.translateTLNoError
              (Expander.envBindings env)
              exprInFunc
          in
          onSuccess (Expander.envBindings env) newBindings simpleforms llvmCode;
          runFunction newBindings immediateFuncName;
          Expander.result (newBindings, [])
        with
          | Expander.IllegalExpression (expr, errors) ->
            Expander.multipleErrors errors
          | exn ->
            Expander.errorFromStringDeprecated (Printexc.to_string exn)
      end
    | _ ->
        Expander.errorFromStringDeprecated (sprintf "expected %s expr" expr.id)

let () =
  at_exit (fun () ->
    if !showTimingStatsAtExit then (
      Profiling.printTimings();
      Indentlexer.printStats();
      flush stdout;
      Zompvm.zompPrintStats());

    if !showStatsAtExit then (
      Stats.statsPrintReport 0));

  Zompvm.zompVerifyCode false;

  let rec step bindings parseState =
    match parseState with
      | Error errors ->
        printf "Parser errors:\n";
        List.iter (printf "%s\n") errors;
        flush stdout;
        step bindings (readExpr bindings)

      | Result [] ->
        step bindings (readExpr bindings)

      | Result (expr :: remExprs) ->
        Compileutils.catchingErrorsDo
          (fun () ->
            if !printAst then begin
              let asString = Ast2.toString expr in
              printf " => %s\n" asString;
            end;

            let newBindings, time = recordTiming
              (fun () ->
                Expander.setTraceMacroExpansion
                  (if !traceMacroExpansion then
                      let trace s e =
                        printf "Expansion step %s:\n%s\n" s (Ast2.toString e)
                      in
                      Some trace
                   else
                      None);
                let newBindings, simpleforms, llvmCode =
                  Compileutils.compileExpr Compileutils.translateTLNoError bindings expr
                in
                onSuccess bindings newBindings simpleforms llvmCode;
                newBindings)
            in
            if (time > !notifyTimeThreshold) then
              printf "Compiling expression took %fs\n" time;
            step newBindings (Result remExprs))
          ~onErrors: (fun errors ->
            List.iter report errors;
            step bindings (Result remExprs))
  in

  let addToplevelBindings bindings = bindings in

  let init() =
    Zompvm.setIsInteractive true;
    Callback.register "parse" parseNativeAst;
    if not (Machine.zompInit()) then begin
      eprintf "Could not initialize ZompVM\n";
      exit(-1);
    end;
    at_exit Machine.zompShutdown;
  in

  let loadPrelude() =
    let defaultMainSrc =
      "std:base:func int main():\n  printString\"error: no main method defined\\n\"\n  std:base:ret 1\nend\n"
    in
    let preludeDir = Filename.dirname Sys.executable_name ^ "/../../../source" in
    Compileutils.catchingErrorsDo
      (fun () -> begin
        let preludeBindings = Compileutils.loadPrelude
          ~appendSource:defaultMainSrc
          preludeDir
        in
        let initialBindings = addToplevelBindings preludeBindings in
        initialBindings
      end)
      ~onErrors:(fun errors ->
        List.iter report errors;
        exit (-2))
  in

  let message msg = printf "%s\n" msg; flush stdout; in

  message (sprintf "Welcome to Zomp shell, version %s%s"
             version
             (if Zompvm.zompIsDebugBuild() then ", Debug build" else ""));

  init();

  let includePath = ref ["."] in
  let handleLLVMCode code = () in
  let translateInclude =
    Expander.makeTranslateIncludeFunction includePath handleLLVMCode
  in
  let addToplevelInstr = Expander.addToplevelInstruction in
  addToplevelInstr "include" "zompSourceFile" translateInclude;

  addToplevelInstr "std:base:run" "statement..." translateRun;
  addToplevelInstr "seq" "ast..." (Expander.makeTranslateSeqFunction handleLLVMCode);

  let dllPath = ref [] in
  let addDllPath path where = addToList dllPath path where in
  addDllPath "." `Back;
  addDllPath "./libs" `Back;
  addDllPath "./tools/external/lib" `Back;
  addToplevelInstr "zmp:compiler:linkclib" "dllFileName"
    (CompilerInstructions.translateLinkCLib dllPath);

  let initialBindings, preludeLoadTime = recordTiming loadPrelude in
  printf "Loading prelude took %.2fs\n" preludeLoadTime;

  message (sprintf "%cx - exit, %chelp - help.\n" toplevelCommandChar toplevelCommandChar);
  let `NoReturn = step initialBindings (Result []) in
  ()


