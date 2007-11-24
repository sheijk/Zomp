
open Ast2
open Printf
open Lang
open Common
open Bindings

let toplevelCommandChar = '!'
let toplevelCommandString = String.make 1 toplevelCommandChar
  
let defaultPrompt = "  # "
and continuedPrompt = "..# "
  
let printLLVMCode = ref false
and llvmEvaluationOn = ref true
  
module StringMap = Map.Make(String)
  
exception AbortExpr

type commandFunc = string list -> Bindings.bindings -> unit
  
let exitCommand _ _  =
  printf "Exiting.\n";
  exit 0

let boolString = function
  | true -> "yes"
  | false -> "no"

let makeToggleCommand refvar message _ _ =
  refvar := not !refvar;
  printf "%s: %s\n" message (boolString !refvar)

let toggleLLVMCommand = makeToggleCommand printLLVMCode "Printing LLVM code"
let toggleEvalCommand = makeToggleCommand llvmEvaluationOn "Evaluating LLVM code"

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
            printf "var %s : %s\n" var.vname (Lang.typeName var.typ)
        | FuncSymbol f ->
            let arg2string (name, typ) = Lang.typeName typ ^ " " ^ name in
            let args = List.map arg2string f.fargs in
            let argString = combine ", " args in
            printf "func %s : %s -> %s\n" f.fname argString (Lang.typeName f.rettype)
        | MacroSymbol m ->
            printf "macro %s\n" m.mname
        | TypedefSymbol t ->
            printf "type %s\n" name
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
            | `Int ->
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

let commands =
  let rec internalCommands = [
    "exit", ["x"; "q"], exitCommand, "Exit";
    "llvm", [], toggleLLVMCommand, "Toggle printing of llvm code";
    "eval", [], toggleEvalCommand, "Toggle evaluation of llvm code";
    "bindings", ["b"], printBindings, "Print a list of defined symbols";
    "run", [], runMain, "Run a function of type 'void (*)(void), default main'";
    "printllvm", ["pl"], (fun _ _ -> Machine.zompPrintModuleCode()), "Print LLVM code in module";
    "load", [], loadCode, "Load code. Supports .ll/.dll/.so/.dylib files";
    "help", ["h"], printHelp, "List all toplevel commands";
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
  
let rec readExpr prompt previousLines bindings =
  printf "%s" prompt;
  flush stdout;
  let line = read_line() in
  if String.length line = 0 then begin
    readExpr prompt previousLines bindings
  end else if line = toplevelCommandString then begin
    printf "Aborted input, cleared \"%s\"\n" previousLines;
    readExpr defaultPrompt "" bindings
  end else if line.[0] = toplevelCommandChar then begin
    handleCommand line bindings;
    readExpr prompt previousLines bindings
  end else begin
    let expr =
      let input = previousLines ^ line ^ "\n" in
      try
        let lexbuf = Lexing.from_string input in
        Sexprparser.main Sexprlexer.token lexbuf
      with Sexprlexer.Eof ->
        readExpr continuedPrompt input bindings
    in
    expr
  end
    
let printWelcomeMessage() = 
  printf "Welcome to the Zomp toplevel.\n";
  printf "%cx - exit, %chelp - help.\n" toplevelCommandChar toplevelCommandChar;
  printf "\n"

          
let () =
  let step bindings () =
    Parseutils.compile
      ~readExpr:(fun bindings -> Some (readExpr defaultPrompt "" bindings))
      ~beforeCompilingExpr:(fun _ -> printf "\n"; flush stdout)
      ~onSuccess:(fun expr oldBindings newBindings simpleforms llvmCode ->
                    let asString = Ast2.expression2string expr in
                    printf " => %s\n" asString;
                    if !printLLVMCode then
                      printf "LLVM code:\n%s\n" llvmCode;
                    if !llvmEvaluationOn then
                      Zompvm.evalLLVMCode oldBindings simpleforms llvmCode;
                 )
      bindings
  in

  let addToplevelBindings bindings = bindings in

  if not (Machine.zompInit()) then begin
    eprintf "Could not initialize ZompVM\n";
    exit(-1);
  end;
  at_exit Machine.zompShutdown;
  printWelcomeMessage();
  let finalBindings = 
    Parseutils.catchingErrorsDo
      (fun () -> begin
         let preludeBindings = Parseutils.loadPrelude "./" in
         let initialBindings = addToplevelBindings preludeBindings in
         step initialBindings ()
       end)
      ~onError:
      (fun () -> begin
         eprintf "Could not load stdlib. Aborting\n";
         exit (-1);
       end)
  in
  ignore finalBindings
  
  (*   let addToplevelBindings bindings = *)
  (*     let macros = [ *)
  (*       "run", *)
  (*       (fun bindings args -> *)
  (*          (match args with *)
  (*            | [{ id = funcName; args = [] }] -> runFunction bindings funcName *)
  (*            | _ -> eprintf "Expected (run functionName)\n"); *)
  (*          seqExpr [] *)
  (*       ); *)
  (*       "do", *)
  (*       (fun bindings args -> *)
  (*          let funcName = "toplevelDo" in *)
  (*          let funcSExpr = *)
  (*            { id = "func"; args = [ *)
  (*                idExpr "void"; *)
  (*                idExpr funcName; *)
  (*                seqExpr []; *)
  (*                seqExpr args; *)
  (*              ] } *)
  (*          in *)
  (*          seqExpr [ *)
  (*            funcSExpr; *)
  (*            simpleExpr "run" [funcName]; *)
  (*          ]) *)
  (*     ] in *)
  (*     List.fold_left *)
  (*       (fun bindings (name, macroF) -> Bindings.addMacro bindings name macroF) *)
  (*       bindings *)
  (*       macros *)
  (*   in *)
