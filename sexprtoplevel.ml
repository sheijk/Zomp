
open Ast2
open Printf
open Lang
  
let printLLVMCode = ref false
  
exception AbortExpr

type commandFunc = Bindings.bindings -> unit
  
let exitCommand _  =
  printf "Exiting.\n";
  exit 0

let toggleLLVMCommand _ =
  printLLVMCode := not !printLLVMCode;
  printf "Printing LLVM code: %s\n" (if !printLLVMCode then "yes" else "no")

let printBindings (bindings :Bindings.bindings) =
  let printSymbol (name, symbol)  =
    match symbol with
      | Bindings.VarSymbol var ->
          printf "var %s : %s\n" var.vname (composedType2String var.typ)
      | Bindings.FuncSymbol f ->
          let arg2string (name, typ) = name ^ " :" ^ composedType2String typ in
          let args = List.map arg2string f.fargs in
          let argString = combine ", " args in
          printf "func %s : %s -> %s" f.fname argString (composedType2String f.rettype)
      | Bindings.MacroSymbol m ->
          printf "macro %s\n" m.mname
      | Bindings.TypedefSymbol t ->
          printf "type %s\n" name
      | Bindings.UndefinedSymbol ->
          printf "undefined %s\n" name
  in
  List.iter printSymbol bindings

module StringMap = Map.Make(String)

let commands =
  let commands = [
    "x", exitCommand, "exit";
    "llvm", toggleLLVMCommand, "toggle printing of llvm code";
    "bindings", printBindings, "print a list of defined symbols";
  ] in
  List.fold_left (fun map (name, func, doc) -> StringMap.add name (func, doc) map) StringMap.empty commands

let handleCommand commandLine bindings =
  let commandName = String.sub commandLine 1 (-1 + String.length commandLine) in
  try
    let (func, _) = StringMap.find commandName commands in
    func bindings
  with
    | Not_found ->
        printf "Error: Could not find command %s.\n" commandName

let listCommands() =
  printf "! to reset (ignore malious input from previous line.\n";
  let printCommand name (_, doc) = printf "!%s - %s\n" name doc in
  StringMap.iter printCommand commands

let rec readExpr previousLines bindings prompt =
  printf "%s" prompt;
  flush stdout;
  let line = read_line() in
  let input = previousLines ^ line in
  if String.length line = 0 then begin
    readExpr previousLines bindings prompt
  end else if line = "?" then begin
    listCommands();
    readExpr previousLines bindings prompt
  end else if line = "!" then begin
    raise AbortExpr
  end else if line.[0] = '!' then begin
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
  printf "!x - exit, ? - help.\n";
  printf "\n"
  
let () =
  printWelcomeMessage();
  let rec step bindings () =
    let goon() = step bindings () in
    begin
      try
        let expr = readExpr "" bindings "cexpr|sexpr # " in
        let asString = Ast2.expression2string expr in
        printf " => %s\n" asString;
        let newBindings, simpleforms = Expander.translateTL bindings expr in
        if !printLLVMCode then begin
          let llvmCodes = List.map Genllvm.gencodeTL simpleforms in
          let llvmCode = combine "\n" llvmCodes in
          printf "LLVM code:\n%s\n" llvmCode
        end;
        step newBindings ()
      with
        | Sexprparser.Error -> printf "parsing error (sexpr).\n"; goon()
        | Sexprlexer.UnknowChar c -> printf "lexer error: encountered unknown character %c.\n" c; goon()
        | Parser2.Error -> printf "parsing error (cexpr).\n"; goon()
        | AbortExpr -> printf "aborted expression, restarting with next line.\n"; goon()
        | Expander.IllegalExpression (_, msg) -> printf "could not translate expression: %s\n" msg; goon()
    end
  in
  step Bindings.defaultBindings ()

  
