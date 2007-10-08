
open Ast2
open Printf
open Lang
let combine = Common.combine
  
let printLLVMCode = ref true
and evalLLVMCode = ref true
  
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
let toggleEvalCommand = makeToggleCommand evalLLVMCode "Evaluating LLVM code"

let matchAnyRegexp patterns =
  match patterns with
    | [] -> Str.regexp ".*"
    | _ ->
        let containsPatterns = List.map (fun p -> ".*" ^ p ^ ".*") patterns in
        Str.regexp ( "\\(" ^ combine "\\|" containsPatterns ^ "\\)" )
      
let printBindings args (bindings :Bindings.bindings) =
  let checkRE = matchAnyRegexp args in
  let printSymbol (name, symbol)  =
    if Str.string_match checkRE name 0 then 
      match symbol with
        | Bindings.VarSymbol var ->
            printf "var %s : %s\n" var.vname (Lang.typeName var.typ)
        | Bindings.FuncSymbol f ->
            let arg2string (name, typ) = Lang.typeName typ ^ " " ^ name in
            let args = List.map arg2string f.fargs in
            let argString = combine ", " args in
            printf "func %s : %s -> %s\n" f.fname argString (Lang.typeName f.rettype)
        | Bindings.MacroSymbol m ->
            printf "macro %s\n" m.mname
        | Bindings.TypedefSymbol t ->
            printf "type %s\n" name
        | Bindings.LabelSymbol { lname = name; } ->
            printf "label %s\n" name
        | Bindings.UndefinedSymbol ->
            printf "undefined %s\n" name
  in
  List.iter printSymbol bindings

let runMain args _ =
  match args with
    | [funcname] -> Machine.zompRunFunction funcname
    | _ -> eprintf "Only one argument allowed\n"; flush stderr

let readFile filename =
  let file = open_in filename in
  let rec read() =
	try
	  let line = input_line file in
	  line ^ "\n" ^ read()
	with
		End_of_file -> ""
  in
  let content = read() in
  close_in file;
  content
    
let loadLLVMFile args _ =
  let loadFile filename =
    try
      let content = readFile filename in
      if not( Machine.zompSendCode content ) then
        eprintf "Could not eval llvm code from file %s\n" filename
    with
        Sys_error message ->
          eprintf "Could not load file %s: %s\n" filename message
  in
  List.iter loadFile args
               
let listCommands commands _ _ =
  printf "# to abort multi-line (continued input)\n";
  let maxCommandLength =
    List.fold_left (fun oldMax (name, _, _, _) -> max oldMax (String.length name)) 5 commands
  in
  let printCommand (name, aliases, _, doc) =
    let aliasString = if List.length aliases > 0 then " (also " ^ combine ", " aliases ^ ")" else "" in
    printf "#%-*s - %s%s\n" (maxCommandLength+1) name doc aliasString
  in
  List.iter printCommand commands

let commands =
  let rec internalCommands = [
    "x", [], exitCommand, "Exit";
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
  let commandRE = Str.regexp "#\\([a-zA-Z0-9]+\\)\\(.*\\)" in
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
  
(* let handleCommand commandLine bindings = *)
(*   let commandName = String.sub commandLine 1 (-1 + String.length commandLine) in *)
(*   try *)
(*     let (func, _) = StringMap.find commandName commands in *)
(*     func bindings *)
(*   with *)
(*     | Not_found -> *)
(*         printf "Error: Could not find command %s.\n" commandName *)

let rec readExpr previousLines bindings prompt =
  printf "%s" prompt;
  flush stdout;
  let line = read_line() in
  let input = previousLines ^ line in
  if String.length line = 0 then begin
    readExpr previousLines bindings prompt
  end else if line = "#" then begin
    raise AbortExpr
  end else if line.[0] = '#' then begin
    handleCommand line bindings;
    readExpr previousLines bindings prompt
  end else begin
    let expr =
      try
        let lexbuf = Lexing.from_string (input ^ " !") in
        Parser2.main Lexer2.token lexbuf
      with _ ->
        begin
          try
            let lexbuf = Lexing.from_string (input ^ "!") in
            Sexprparser.main Sexprlexer.token lexbuf
          with _ ->
            readExpr input bindings "cont. inp.  # "
        end
    in
    expr
  end
    
let printWelcomeMessage() = 
  printf "Welcome to the Zomp toplevel.\n";
  printf "#x - exit, #help - help.\n";
  printf "\n"

let () =
  if not (Machine.zompInit()) then begin
    eprintf "Could not initialize ZompVM\n";
    exit(-1);
  end;
  at_exit Machine.zompShutdown;
  printWelcomeMessage();
  let rec step bindings () =
    let goon() = step bindings () in
    begin
      try
        let expr = readExpr "" bindings "cexpr|sexpr # " in
        let asString = Ast2.expression2string expr in
        printf " => %s\n" asString;
        let newBindings, simpleforms = Expander.translateTL bindings expr in
        let llvmCodes = List.map Genllvm.gencodeTL simpleforms in
        let llvmCode = combine "\n" llvmCodes in
        if !printLLVMCode then begin
          printf "LLVM code:\n%s\n" llvmCode; flush stdout;
        end;
        if !evalLLVMCode then begin
          if not( Machine.zompSendCode llvmCode ) then
            eprintf "Error evaluating code\n"; flush stderr;
        end;
        step newBindings ()
      with
        | Sexprparser.Error -> printf "parsing error (sexpr).\n"; goon()
        | Sexprlexer.UnknowChar c ->
            printf "Lexer error: encountered unknown character %s.\n" c;
            goon()
        | Parser2.Error ->
            printf "Parsing error (cexpr).\n"; goon()
        | AbortExpr ->
            printf "Aborted expression, restarting with next line.\n";
            goon()
        | Expander.IllegalExpression (expr, msg) ->
            printf "Could not translate expression: %s\nexpr: %s\n" msg (Ast2.expression2string expr);
            goon()
        | Lang.CouldNotParseType descr ->
            printf "Unknown type: %s\n" descr;
            goon()
    end
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
    let simpleforms = Parseutils.parse parseF lexbuf Bindings.defaultBindings [] in
    List.fold_left
      (fun bindings form ->
         match form with
           | GlobalVar var -> Bindings.addVar bindings var
           | DefineFunc func -> Bindings.addFunc bindings func)
      Genllvm.defaultBindings
      simpleforms
  in
  let initialBindings = loadPrelude() in
(*   let _ = loadPrelude in *)
(*   let initialBindings = Bindings.defaultBindings in *)
  step initialBindings ()
  
