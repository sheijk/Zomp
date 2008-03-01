
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
and printSExpr = ref false
and printDeclarations = ref true
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

let toggleSExprCommand = makeToggleCommand printSExpr "Printing s-expressions"
let toggleLLVMCommand = makeToggleCommand printLLVMCode "Printing LLVM code"
let togglePrintDeclarations = makeToggleCommand printDeclarations "Printing declarations"
let toggleEvalCommand = makeToggleCommand llvmEvaluationOn "Evaluating LLVM code"

let parseSExpr input =
  try
    let lexbuf = Lexing.from_string input in
    Some( Sexprparser.main Sexprlexer.token lexbuf )
  with Sexprlexer.Eof ->
    None

let parseIExpr source =
  if String.length source >= 3 && Str.last_chars source 3 = "\n\n\n" then
    try
      let lexbuf = Lexing.from_string source in
      let lexstate = Iexprtest.lexbufFromString "dummy.zomp" source in
      let lexFunc = Newparsertest.lexFunc lexstate in
      let rec read acc =
        try
          let expr = Newparser.main lexFunc lexbuf in
          read (expr :: acc)
        with
          | End_of_file -> acc
      in
      match List.rev (read []) with
        | [singleExpr] ->
            Some singleExpr
        | multipleExprs ->
            Some { Ast2.id = "opseq"; args = multipleExprs }
    with _ ->
      None
  else (
    None)

let parseFunc = ref parseSExpr

let toggleParseFunc args _ =
  match args with
    | ["sexpr"] -> parseFunc := parseSExpr
    | ["indent"] -> parseFunc := parseIExpr
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
    "printSExpr", [], toggleSExprCommand, "Toggle printing of parsed s-expressions";
    "printDecl", [], togglePrintDeclarations, "Toggle printing declarations";
    "bindings", ["b"], printBindings, "Print a list of defined symbols";
    "run", [], runMain, "Run a function of type 'void (*)(void), default main'";
    "printllvm", ["pl"], (fun _ _ -> Machine.zompPrintModuleCode()), "Print LLVM code in module";
    "verify", ["v"], toggleVerifyCommand, "Verify generated llvm code";
    "load", [], loadCode, "Load code. Supports .ll/.dll/.so/.dylib files";
    "writeSymbols", [], writeSymbols, "Write all symbols to given file for emacs eldoc-mode";
    "syntax", [], toggleParseFunc, "Choose a syntax";
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

let hiddenCommandPrefix = "!silent"

let beginsWith line word =
  let wordLength = String.length word
  and lineLength = String.length line
  in
  lineLength >= wordLength &&
    (String.sub line 0 (String.length word)) = word
      
let removeBeginning text count =
  let textLength = String.length text in
  let count = min textLength count in
  String.sub text count (textLength - count)

let rec readExpr prompt previousLines bindings =
  printf "%s" prompt;
  flush stdout;
  let line = read_line() in
  (*   if String.length line = 0 then begin *)
  (*     readExpr prompt previousLines bindings *)
  (*   end else *)
  if beginsWith line hiddenCommandPrefix then begin
    let command = removeBeginning line (String.length hiddenCommandPrefix + 1) in
    handleCommand command bindings;
    readExpr "" previousLines bindings
  end else if line = toplevelCommandString then begin
    printf "Aborted input, cleared \"%s\"\n" previousLines;
    readExpr defaultPrompt "" bindings
  end else if String.length line > 0 && line.[0] = toplevelCommandChar then begin
    handleCommand line bindings;
    readExpr prompt previousLines bindings
  end else begin
    let expr =
      let input = previousLines ^ line ^ "\n" in
      match !parseFunc input with
        | Some expr -> expr
        | None -> readExpr continuedPrompt input bindings
            (*       try *)
            (*         let lexbuf = Lexing.from_string input in *)
            (*         Sexprparser.main Sexprlexer.token lexbuf *)
            (*       with Sexprlexer.Eof -> *)
            (*         readExpr continuedPrompt input bindings *)
    in
    expr
  end
    
let printWelcomeMessage() = 
  printf "Welcome to the Zomp toplevel.\n";
  printf "%cx - exit, %chelp - help.\n" toplevelCommandChar toplevelCommandChar;
  printf "\n"

let () =
  let rec step bindings () =
    Parseutils.catchingErrorsDo
      (fun () -> begin
         let expr = readExpr defaultPrompt "" bindings in

         let onSuccess newBindings simpleforms llvmCode =
           if !printSExpr then begin
             let asString = Ast2.expression2string expr in
             printf " => %s\n" asString;
           end;
           
           if !printLLVMCode then
             printf "LLVM code:\n%s\n" llvmCode;
           
           if !llvmEvaluationOn then
             Zompvm.evalLLVMCode bindings simpleforms llvmCode;
           
           if !printDeclarations then begin
             List.iter (fun form ->
                          let text = Lang.toplevelFormToString form in
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
             
         with _ as originalException ->
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
      (fun msg -> begin
         printf "%s" msg;
         eprintf "Could not load stdlib. Aborting\n";
         exit (-1);
       end)
  in
  ignore finalBindings


    
