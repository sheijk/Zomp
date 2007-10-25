
open Ast2
open Printf
open Lang
open Common
open Bindings

open Zompvm

let toplevelCommandChar = '!'
let toplevelCommandString = String.make 1 toplevelCommandChar
  
let prompt =          "  # "
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
        let containsPatterns = List.map (fun p -> ".*" ^ p ^ ".*") patterns in
        Str.regexp ( "\\(" ^ combine "\\|" containsPatterns ^ "\\)" )
      
let printBindings args (bindings :bindings) =
  let checkRE = matchAnyRegexp args in
  let printSymbol (name, symbol)  =
    if Str.string_match checkRE name 0 then 
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

let runMain args bindings =
  match args with
    | [funcname] ->
        begin
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
                    | `Pointer `Char ->
                        let retval = Machine.zompRunFunctionString funcname in
                        printf " => %s\n" retval
                    | otherRetType ->
                        eprintf "Cannot call a function which returns %s\n" (Typesystems.Zomp.typeName otherRetType)
                end
            | _ ->
                eprintf "Cannot run %s because no such function was found\n" funcname
        end
    | _ ->
        eprintf "Only one argument allowed\n"; flush stderr
    
let loadLLVMFile args _ =
  let loadFile filename =
    try
      let content = readFile filename in
      if not( Machine.zompSendCode content "" ) then
        eprintf "Could not eval llvm code from file %s\n" filename
    with
        Sys_error message ->
          eprintf "Could not load file %s: %s\n" filename message
  in
  List.iter loadFile args
               
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
    "load", [], loadLLVMFile, "Load file containing LLVM code (*.ll)";
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
  
let rec readExpr previousLines bindings prompt =
  printf "%s" prompt;
  flush stdout;
  let line = read_line() ^ "\n" in
  if String.length line = 0 then begin
    readExpr previousLines bindings prompt
  end else if line = toplevelCommandString then begin
    raise AbortExpr
  end else if line.[0] = toplevelCommandChar then begin
    handleCommand line bindings;
    readExpr previousLines bindings prompt
  end else begin
    let expr =
      let input = previousLines ^ line in
      try
        let lexbuf = Lexing.from_string input in
        Sexprparser.main Sexprlexer.token lexbuf
      with Sexprlexer.Eof ->
        readExpr input bindings continuedPrompt
    in
    expr
  end
    
let printWelcomeMessage() = 
  printf "Welcome to the Zomp toplevel.\n";
  printf "%cx - exit, %chelp - help.\n" toplevelCommandChar toplevelCommandChar;
  printf "\n"

let rec doAll ~onError ~onSuccess = function
  | [] -> onSuccess()
  | (f, errorMsg) :: rem ->
      if f() then
        doAll ~onError ~onSuccess rem
      else
        onError errorMsg
      
let catchingErrorsDo f ~onError =
  let wasOk = ref false in
  begin try
    f();
    wasOk := true
  with
    | Sexprparser.Error ->
        printf "parsing error (sexpr).\n"
    | Sexprlexer.UnknowChar c ->
        printf "Lexer error: encountered unknown character %s.\n" c
    | Parser2.Error ->
        printf "Parsing error (cexpr).\n"
    | AbortExpr ->
        printf "Aborted expression, restarting with next line.\n"
    | Expander.IllegalExpression (expr, msg) ->
        printf "Could not translate expression: %s\nexpr: %s\n" msg (Ast2.expression2string expr)
    | Lang.CouldNotParseType descr ->
        printf "Unknown type: %s\n" descr
    | FailedToEvaluateLLVMCode (llvmCode, errorMsg) ->
        printf "Could not evaluate LLVM code: %s\n%s\n" errorMsg llvmCode
  end;
  if not !wasOk then
    onError()
      
let () =
  let rec step bindings () =
    begin
      catchingErrorsDo
        (fun () -> begin
           let expr = readExpr "" bindings prompt in
           let asString = Ast2.expression2string expr in
           printf " => %s\n" asString;
           let newBindings, simpleforms, llvmCode = Zompvm.compileExpr Expander.translateTL bindings expr in
           if !printLLVMCode then begin
             printf "LLVM code:\n%s\n" llvmCode; flush stdout;
           end;
           if !llvmEvaluationOn then begin
             evalLLVMCode bindings simpleforms llvmCode
           end;
           step newBindings ()
         end)
        ~onError:(step bindings)
    end
  in
  let rec parse parseF (lexbuf :Lexing.lexbuf) codeAccum =
    try
      let expr = parseF lexbuf in
      parse parseF lexbuf (codeAccum @ [expr])
    with
      | Lexer2.Eof | Sexprlexer.Eof -> codeAccum
  in
  let loadPrelude () =
    let llvmPreludeFile = "stdlib.ll"
    and zompPreludeFile = "stdlib.zomp"
    in
    printf "Loading LLVM prelude from %s\n" llvmPreludeFile; flush stdout;
    loadLLVMFile [llvmPreludeFile] (Bindings.defaultBindings);
    printf "Loading Zomp prelude from %s\n" zompPreludeFile; flush stdout;
    let lexbuf = Lexing.from_channel (open_in zompPreludeFile) in
    let parseF = Sexprparser.main Sexprlexer.token in
    let exprs = parse parseF lexbuf [] in
    let newBindings =
      List.fold_left
        (fun bindings expr ->
           let newBindings, simpleforms, llvmCode = compileExpr Expander.translateTL bindings expr in
           evalLLVMCode bindings simpleforms llvmCode;
           newBindings)
        Genllvm.defaultBindings
        exprs
    in
    newBindings
  in

  if not (Machine.zompInit()) then begin
    eprintf "Could not initialize ZompVM\n";
    exit(-1);
  end;
  at_exit Machine.zompShutdown;
  printWelcomeMessage();
  catchingErrorsDo
    (fun () -> begin
       let initialBindings = loadPrelude() in
       step initialBindings ()
     end)
    ~onError:
    (fun () -> begin
       eprintf "Could not load stdlib. Aborting\n";
       exit (-1);
     end)
  
