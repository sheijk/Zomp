
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

module StringMap = Map.Make(String)

exception AbortExpr

type commandFunc = string list -> Bindings.bindings -> unit

let exitCommand _ _  =
  printf "Exiting.\n";
  exit 0

let promptCommand args _ =
  let report() =
    printf "Set prompt to '%s' and cont. prompt to '%s'\n" !defaultPrompt !continuedPrompt
  in
  let removeQuotes quoteChar str =
    match dequoteString quoteChar str with | `NotQuoted str | `Quoted str -> str
  in
  match List.map (removeQuotes '\'' ++ removeQuotes '"') args with
    | [] ->
        defaultPrompt := "";
        continuedPrompt := "";
        report()
    | [newPrompt] ->
        defaultPrompt := "  " ^ newPrompt ^ " ";
        continuedPrompt := ".." ^ newPrompt ^ " ";
        report()
    | [newDefault; newContinued] ->
        defaultPrompt := newDefault;
        continuedPrompt := newContinued;
        report()
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

let parseFunc = ref Parseutils.parseSExpr

let toggleParseFunc args _ =
  let confirm syntax = printf "Changed syntax to %s\n" syntax in
  match args with
    | ["sexpr"] -> parseFunc := Parseutils.parseSExpr; confirm "sexpr"
    | ["indent"] -> parseFunc := Parseutils.parseIExpr; confirm "indent"
    | _ -> printf "Invalid option. Use sexpr or indent\n"

let toggleVerifyCommand args _ =
  match args with
    | ["on"] -> Zompvm.zompVerifyCode true
    | ["off"] -> Zompvm.zompVerifyCode false
    | _ -> eprintf "Expected on|off\n"; flush stderr

let matchAnyRegexp patterns =
  match patterns with
    | [] -> Str.regexp ".*"
    | _ ->
        let containsPatterns = List.map (String.lowercase ++ sprintf ".*%s.*") patterns in
        Str.regexp ( "\\(" ^ combine "\\|" containsPatterns ^ "\\)" )

let printBindings args (bindings :bindings) =
  let checkRE = matchAnyRegexp args in
  let printSymbol (name, symbol)  =
    if Str.string_match checkRE (String.lowercase name) 0 then
      match symbol with
        | VarSymbol var ->
            printf "var %s %s\n" (Lang.typeName var.typ) var.vname
        | FuncSymbol f ->
            let arg2string (name, typ) = Lang.typeName typ ^ " " ^ name in
            let args = List.map arg2string f.fargs in
            let argString = combine ", " args in
            printf "func %s %s(%s)\n" (Lang.typeName f.rettype) f.fname argString
        | MacroSymbol m ->
            printf "macro %s %s\n" m.mname m.mdocstring
        | TypedefSymbol t ->
            printf "type %s = %s\n" name (typeName t)
        | LabelSymbol { lname = name; } ->
            printf "label %s\n" name
        | UndefinedSymbol ->
            printf "undefined %s\n" name
  in
  print_newline();
  List.iter printSymbol bindings;
  print_newline()

let runFunction bindings funcname =
  printf "\n"; flush stdout;
  match Bindings.lookup bindings funcname with
    | FuncSymbol func ->
        begin
          match func.rettype with
            | `Void ->
                Machine.zompRunFunction funcname;
                printf " => void\n";
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

let loadCDll pathAndName _ =
  let handle = Zompvm.zompLoadLib pathAndName in
  if handle = 0 then
    printf "Could not load %s\n" pathAndName
  else
    printf "Succesfully loaded %s\n" pathAndName

let loadCode args bindings =
  let matches re string = Str.string_match (Str.regexp re) string 0 in
  List.iter
    (fun name ->
       if matches ".*\\.ll" name then
         loadLLVMFile name bindings
       else if matches ".*\\.\\(dylib\\|so\\|dll\\)" name then
         loadCDll name bindings
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
            let typeName = Expander.findTypeName bindings in
            print "Symbol table\n";
            List.iter (fun (name, symbol) ->
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
                               fprintf stream "type %s" (typeName typ)
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
          eprintf "Expecting one argument\n";
          flush stderr
        end

let commands =
  let rec internalCommands = [
    "exit", ["x"; "q"], exitCommand, "Exit";
    "llvm", [], toggleLLVMCommand, "Toggle printing of llvm code";
    "eval", [], toggleEvalCommand, "Toggle evaluation of llvm code";
    "printAst", [], toggleAstCommand, "Toggle printing of parsed s-expressions";
    "printDecl", [], togglePrintDeclarations, "Toggle printing declarations";
    "bindings", ["b"], printBindings, "Print a list of defined symbols";
    "run", [], runMain, "Run a function of type 'void (*)(void), default main'";
    "printllvm", ["pl"], (fun _ _ -> Machine.zompPrintModuleCode()), "Print LLVM code in module";
    "verify", ["v"], toggleVerifyCommand, "Verify generated llvm code";
    "load", [], loadCode, "Load code. Supports .ll/.dll/.so/.dylib files";
    "writeSymbols", [], writeSymbols, "Write all symbols to given file for emacs eldoc-mode";
    "syntax", [], toggleParseFunc, "Choose a syntax";
    "help", ["h"], printHelp, "List all toplevel commands";
    "prompt", [], promptCommand, "Set prompt";
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

let rec readExpr prompt previousLines bindings =
  printf "%s" prompt; flush stdout;

  let line = read_line() in

  if line |> beginsWith silentPrefix then begin
    let command = removeBeginning line (String.length silentPrefix + 1) in
    handleCommand command bindings;
    readExpr "" previousLines bindings

  end else if line = toplevelCommandString then begin
    printf "Aborted input, cleared \"%s\"\n" previousLines;
    readExpr !defaultPrompt "" bindings

  end else if nthChar 0 line = Some toplevelCommandChar then begin
    handleCommand line bindings;
    readExpr prompt previousLines bindings

  end else begin
    let expr =
      let input = previousLines ^ line ^ "\n" in
      match !parseFunc input with
        | Some expr -> expr
        | None -> readExpr !continuedPrompt input bindings
    in
    expr
  end

let () =
  let rec step bindings () =
    Compileutils.catchingErrorsDo
      (fun () -> begin
         let expr = readExpr !defaultPrompt "" bindings in

         let onSuccess newBindings simpleforms llvmCode =
           if !printAst then begin
             let asString = Ast2.expression2string expr in
             printf " => %s\n" asString;
           end;

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
           flush stdout;
         in

         try
           let newBindings, simpleforms, llvmCode =
             Parseutils.compileExpr Expander.translateTL bindings expr
           in
           onSuccess newBindings simpleforms llvmCode;
           step newBindings ()

         with originalException ->
           printf "Running immediately\n";

           let immediateFuncName = "toplevel:immediate" in

           let exprInFunc =
             { id = "func"; args = [
                 idExpr "void";
                 idExpr immediateFuncName;
                 seqExpr [];
                 seqExpr [expr]
               ] }
           in
           try
             let newBindings, simpleforms, llvmCode =
               Parseutils.compileExpr Expander.translateTL bindings exprInFunc
             in
             onSuccess newBindings simpleforms llvmCode;
             runFunction newBindings immediateFuncName;
             step newBindings ()
           with _ ->
             raise originalException
       end)
      ~onError: (fun msg ->
                   printf "%s" msg;
                   step bindings ()
                )
  in

  let addToplevelBindings bindings = bindings in

  let init() =
    if not (Machine.zompInit()) then begin
      eprintf "Could not initialize ZompVM\n";
      exit(-1);
    end;
    at_exit Machine.zompShutdown;
  in

  let loadPrelude() =
    Compileutils.catchingErrorsDo
      (fun () -> begin
         let preludeBindings = Compileutils.loadPrelude "./" in
         let initialBindings = addToplevelBindings preludeBindings in
         initialBindings
       end)
      ~onError:
      (fun msg -> begin
         printf "%s" msg;
         eprintf "Could not load stdlib. Aborting\n";
         exit (-2);
       end)
  in

  let run bindings = step bindings () in

  let message msg = printf "%s\n" msg; flush stdout; in

  message "Welcome to the interactive ZompVM";

  message "Initializing...";
  init();

  let recordTiming name f =
    let startTime = Sys.time() in
    let result = f() in
    let endTime = Sys.time() in
    printf "%s took %fs\n" name (endTime -. startTime);
    result
  in

  message "Loading prelude...";
  let initialBindings = recordTiming "Loading prelude" loadPrelude in

  message (sprintf "%cx - exit, %chelp - help.\n" toplevelCommandChar toplevelCommandChar);
  run initialBindings



