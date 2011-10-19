
open Ast2
open Printf
open Lang
open Common
open Bindings

let toplevelCommandChar = '!'
let toplevelCommandString = String.make 1 toplevelCommandChar

let defaultPrompt = ref "  # "
and continuedPrompt = ref "..# "

let printLLVMCode = ref false
and printAst = ref false
and printDeclarations = ref true
and llvmEvaluationOn = ref true
and printForms = ref false
and showStatsAtExit = ref true

module StringMap = Map.Make(String)

exception AbortExpr

type commandFunc = string list -> Bindings.bindings -> unit

let errorMessage msg =
  eprintf "%s\n" msg;
  flush stderr

let exitCommand _ _  =
  printf "Exiting.\n";
  exit 0

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
        printf "Expected 0-2 arguments instead of %d\n" (List.length args)

let boolString = function
  | true -> "yes"
  | false -> "no"

let makeToggleCommand refvar message _ _ =
  refvar := not !refvar;
  printf "%s: %s\n" message (boolString !refvar)

let toggleAstCommand = makeToggleCommand printAst "Printing s-expressions"
let toggleLLVMCommand = makeToggleCommand printLLVMCode "Printing LLVM code"
let togglePrintDeclarations = makeToggleCommand printDeclarations "Printing declarations"
let toggleEvalCommand = makeToggleCommand llvmEvaluationOn "Evaluating LLVM code"
let togglePrintForms = makeToggleCommand printForms "Print translated forms"
let toggleShowStatsAtExit = makeToggleCommand showStatsAtExit "Show stats at exit"

let parseFunc = ref Parseutils.parseIExpr

let toggleParseFunc args _ =
  let confirm syntax = printf "Changed syntax to %s\n" syntax in
  match args with
    | ["indent"] -> parseFunc := Parseutils.parseIExpr; confirm "indent"
    | _ -> printf "Invalid option. Use sexpr or indent\n"

let makeToggleCppCommand setF =
  fun args (_:bindings) ->
    match args with
      | ["on"] -> setF true
      | ["off"] -> setF false
      | _ ->
          errorMessage "Expected on|off"

let toggleVerifyCommand = makeToggleCppCommand (fun b -> Zompvm.zompVerifyCode b)
let toggleOptimizeFunctionCommand =
  makeToggleCppCommand (fun b -> Zompvm.zompSetOptimizeFunction b)

let notifyTimeThreshold = ref 0.5

let setNotifyTimeThresholdCommand args _ =
  match args with
    | [timeStr] ->
        begin try
          let newTime = float_of_string timeStr in
          if newTime >= 0.0 then
            notifyTimeThreshold := newTime
          else
            printf "Given time must not be less than zero\n"
        with Failure "float_of_string" ->
          printf "Could not parse '%s' as float\n" timeStr
        end
    | _ ->
        printf "Expected only one argument\n"

let matchAnyRegexp patterns =
  match patterns with
    | [] -> Str.regexp ".*"
    | _ ->
        let containsPatterns = List.map (String.lowercase ++ sprintf ".*%s.*") patterns in
        Str.regexp ( "\\(" ^ combine "\\|" containsPatterns ^ "\\)" )

let printBindings args (bindings :bindings) =
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
                printf "Cannot call a function which returns %s\n" (Typesystems.Zomp.typeName otherRetType)
        end
    | _ ->
        eprintf "Cannot run %s because no such function was found\n" funcname

let runMain args bindings =
  match args with
    | [funcname] ->
        runFunction bindings funcname
    | _ ->
        eprintf "Only one argument allowed\n"; flush stderr

let loadLLVMFile filename _ =
  Zompvm.loadLLVMFile filename

let loadCode args bindings =
  let matches re string = Str.string_match (Str.regexp re) string 0 in
  List.iter
    (fun name ->
       if matches ".*\\.ll" name then
         loadLLVMFile name bindings
       else
         printf "Unsupported file extension\n" )
    args

let listCommands commands _ _ =
  printf "%c to abort multi-line (continued input)\n" toplevelCommandChar;
  let maxCommandLength =
    List.fold_left (fun oldMax (name, _, _, _) -> max oldMax (String.length name)) 5 commands
  in
  let printCommand (name, aliases, _, doc) =
    let aliasString = if List.length aliases > 0 then " (also " ^ combine ", " aliases ^ ")" else "" in
    printf "!%-*s - %s%s\n" (maxCommandLength+1) name doc aliasString
  in
  List.iter printCommand commands

let writeSymbols args (bindings : Bindings.bindings) =
  match args with
    | [fileName] ->
        begin
          if Sys.file_exists fileName then
            Sys.remove fileName;
          let stream = open_out fileName in
          try
            let print = fprintf stream in
            print "Symbol table\n";
            Bindings.iter (fun (name, symbol) ->
                         fprintf stream "%s =" name;
                         begin match symbol with
                           | VarSymbol var ->
                               fprintf stream "var of type %s"
                                 (typeName var.typ);
                           | FuncSymbol func ->
                               let argToString (name, typ) =
                                 sprintf "%s %s" (typeName typ) name
                               in
                               let args = List.map argToString func.fargs in
                               let argString = Common.combine ", " args in
                               fprintf stream "(%s) -> %s"
                                 argString
                                 (typeName func.rettype);
                           | MacroSymbol macro ->
                               fprintf stream "%s" macro.mdocstring;
                           | LabelSymbol label ->
                               fprintf stream "label %s" label.lname;
                           | TypedefSymbol typ ->
                               fprintf stream "type %s" (typeDescr typ)
                           | UndefinedSymbol ->
                               fprintf stream "undefined"
                         end;
                         fprintf stream "\n" )
              bindings;
            let builtinDoc (name, params) = fprintf stream "%s =%s\n" name params in
            List.iter builtinDoc [
              "var", "type name default";
              "func", "returnType name ((typ0 arg0)...) implExp";
              "assign", "lvar value";
              "seq", "expr...";
              "type", "name typeExpr";
              "ptr", "var";
              "ret", "expr";
              "label", "name";
              "branch", "label | boolExpr trueLabel falseLabel";
              "macro", "args* implExpr";
              "fieldptr", "recordExpr fieldName";
              "load", "pointerExpr";
              "store", "pointerExpr valueExpr";
              "nullptr", "type";
              "ptradd", "pointerExpr intExpr";
              "malloc", "type count?";
              "cast", "type value";
              "include", "filename";
            ];
            close_out stream
          with _ ->
            close_out stream
        end
    | _ ->
        begin
          eprintf "Expected one argument\n";
          flush stderr
        end

let optimizeCommand args (_:bindings) =
  match args with
    | [] -> begin
        Machine.zompOptimizeFunctions();
        printf "Ran optimizations on all functions\n";
        flush stdout;
      end
    | _ ->
        errorMessage "Expecting zero arguments\n"

let writeLLVMCodeToFileCommand args (_:bindings) =
  match args with
    | [fileName] -> begin
        Machine.zompWriteLLVMCodeToFile fileName;
        printf "Wrote LLVM code to file %s\n" fileName;
        flush stdout;
      end
    | _ ->
        errorMessage "Expected one argument: file name"

let version = "0.?"

let printVersionInfo args (_:bindings) =
  match args with
      [] ->
        printf "Version %s, build %s\n" version (Zompvm.zompBuildInfo());
        flush stdout
    | _ ->
        printf "Expected no arguments";
        flush stdout
          
let commands =
  let rec internalCommands = [
    "bindings", ["b"], printBindings, "Print a list of defined symbols";
    "eval", [], toggleEvalCommand, "Toggle evaluation of llvm code";
    "exit", ["x"; "q"], exitCommand, "Exit";
    "help", ["h"], printHelp, "List all toplevel commands";
    "llvm", [], toggleLLVMCommand, "Toggle printing of llvm code";
    "load", [], loadCode, "Load code. Supports .ll/.dll/.so/.dylib files";
    "optimize", [], optimizeCommand, "Optimize all functions";
    "printAst", [], toggleAstCommand, "Toggle printing of parsed s-expressions";
    "printBaseLang", [], togglePrintForms, "Toggle printing translated base lang forms";
    "printDecl", [], togglePrintDeclarations, "Toggle printing declarations";
    "printllvm", ["pl"], (fun _ _ -> Machine.zompPrintModuleCode()), "Print LLVM code in module";
    "prompt", [], changePromptCommand, "Set prompt";
    "run", [], runMain, "Run a function of type 'void (*)(void), default main'";
    "setNotifyTimeThresholdCommand", [], setNotifyTimeThresholdCommand, "Set minimum compilation time to print timing information";
    "setOptimizeFunctions", [], toggleOptimizeFunctionCommand, "Optimize functions on definition";
    "showStatsAtExit", [], toggleShowStatsAtExit, "Show stats at exit";
    "syntax", [], toggleParseFunc, "Choose a syntax";
    "verify", ["v"], toggleVerifyCommand, "Verify generated llvm code";
    "version", [], printVersionInfo, "Print version/build info";
    "writeSymbols", [], writeSymbols, "Write all symbols to given file for emacs eldoc-mode";
    "writellvm", [], writeLLVMCodeToFileCommand, "Write LLVM code to file";
  ]
  and printHelp ignored1 ignored2 = listCommands internalCommands ignored1 ignored2
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
          printf "Error: Could not find command %s.\n" commandName
  else
    printf "Not a command string: '%s'\n" commandLine

let silentPrefix = "!silent"

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

let rec readExpr continued previousLines bindings =
  let prompt = match continued with
    | `NotContinued -> !defaultPrompt
    | `Continued -> !continuedPrompt
    | `NoPrompt -> ""
  in
  printf "%s" prompt; flush stdout;

  let line = read_line() in

  if line |> beginsWith silentPrefix then begin
    let command = removeBeginning line (String.length silentPrefix + 1) in
    handleCommand command bindings;
    readExpr `NoPrompt previousLines bindings

  end else if line = toplevelCommandString then begin
    printf "Aborted input, cleared \"%s\"\n" previousLines;
    readExpr `NotContinued "" bindings

  end else if nthChar 0 line = Some toplevelCommandChar then begin
    handleCommand line bindings;
    readExpr continued previousLines bindings

  end else begin
    let expr =
      let input = previousLines ^ line ^ "\n" in
      match !parseFunc input with
        | Some expr -> expr
        | None ->
            if isWhitespaceString input then
              readExpr continued "" bindings
            else
              readExpr `Continued input bindings
    in
    expr
  end

let recordTiming f =
  let startTime = Sys.time() in
  let result = f() in
  let endTime = Sys.time() in
  result, (endTime -. startTime)


module CompilerInstructions =
struct
  open Expander
  open Ast2

  let translateLinkCLib env = function
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
            let handle = Zompvm.zompLoadLib fileName in
            if handle = 0 then
              Expander.errorFromString (sprintf "Could not load C library '%s'\n" fileName)
            else
              Expander.tlReturnNoExprs env
        end
    | invalidExpr ->
        Expander.errorFromString
          (sprintf "Expecting '%s fileName" invalidExpr.Ast2.id)
end

let parse str =
  let expr =
    match Parseutils.parseIExprs str with
      | Parseutils.Exprs [expr] -> expr
      | Parseutils.Exprs exprs -> Ast2.seqExpr exprs
      | Parseutils.Error error ->
          let msg = Parseutils.parseErrorToString error in
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
              Ast2.expr macroReturn [callExpr [idExpr "void"]]]
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
          | exn ->
              Expander.errorFromString (Printexc.to_string exn)
      end
    | _ ->
        Expander.errorFromString (sprintf "Expected %s expr" expr.id)

let () =
  at_exit (fun () ->
             if !showStatsAtExit then (
               Profiling.printTimings();
               Indentlexer.printStats();
               flush stdout;
               Zompvm.zompPrintStats()));
  Zompvm.zompVerifyCode false;
  let rec step bindings () =
    Compileutils.catchingErrorsDo
      (fun () -> begin
         let expr = readExpr `NotContinued "" bindings in

         if !printAst then begin
           let asString = Ast2.expression2string expr in
           printf " => %s\n" asString;
         end;

         let newBindings, time = recordTiming
           (fun () ->
              let newBindings, simpleforms, llvmCode =
                Compileutils.compileExpr Compileutils.translateTLNoError bindings expr
              in
              onSuccess bindings newBindings simpleforms llvmCode;
              newBindings)
         in
         if (time > !notifyTimeThreshold) then
           printf "Compiling expression took %fs\n" time;
         step newBindings ()
       end)
      ~onError: (fun msg ->
                   printf "%s" msg;
                   step bindings ()
                )
  in

  let addToplevelBindings bindings = bindings in

  let init() =
    Zompvm.setIsInteractive true;
    Callback.register "parse" parse;
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
    Compileutils.catchingErrorsDo
      (fun () -> begin
         let preludeBindings = Compileutils.loadPrelude
           ~appendSource:defaultMainSrc
           "./"
         in
         let initialBindings = addToplevelBindings preludeBindings in
         initialBindings
       end)
      ~onError:
      (fun msg -> begin
         printf "%s" msg;
         eprintf "Could not load prelude. Aborting\n";
         exit (-2);
       end)
  in

  let run bindings = step bindings () in

  let message msg = printf "%s\n" msg; flush stdout; in

  message "Welcome to the interactive ZompVM";
  message (sprintf "Version %s%s"
             version
             (if Zompvm.zompIsDebugBuild() then ", Debug build" else ""));

  message "Initializing...";
  init();

  let includePath = ref ["."] in
  let handleLLVMCode code = () in
  let translateInclude =
    Expander.makeTranslateIncludeFunction includePath handleLLVMCode
  in
  let addToplevelInstr = Expander.addToplevelInstruction in
  addToplevelInstr "include" translateInclude;
  addToplevelInstr "std:base:run" translateRun;
  addToplevelInstr "seq" (Expander.makeTranslateSeqFunction handleLLVMCode);
  addToplevelInstr "zmp:compiler:linkclib" CompilerInstructions.translateLinkCLib;

  message "Loading prelude...";
  let initialBindings, preludeLoadTime = recordTiming loadPrelude in
  printf "Loading prelude took %fs\n" preludeLoadTime;

  message (sprintf "%cx - exit, %chelp - help.\n" toplevelCommandChar toplevelCommandChar);
  run initialBindings

