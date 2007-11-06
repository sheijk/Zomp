
open Common
open Printf
  
let rec parse parseF (lexbuf :Lexing.lexbuf) bindings codeAccum =
  try
    let expr = parseF lexbuf in
(*     let exprString = Ast2.expression2string expr in *)
(*     let exprComment = commentOut "; " exprString in *)
(*     printf "%s\n" exprComment; *)
    let newBindings, simpleforms = Expander.translateTL bindings expr in
    parse parseF lexbuf newBindings (codeAccum @ simpleforms)
  with
    | Lexer2.Eof | Sexprlexer.Eof -> bindings, codeAccum

exception CatchedError of string
let signalError msg = raise (CatchedError msg)
  
let catchingErrorsDo f ~onError =
  try
    begin try
      f()
    with
      | Sexprparser.Error ->
          signalError (sprintf "parsing error (sexpr).\n")
      | Sexprlexer.UnknowChar c ->
          signalError (sprintf "Lexer error: encountered unknown character %s.\n" c)
      | Parser2.Error ->
          signalError (sprintf "Parsing error (cexpr).\n")
            (*     | AbortExpr -> *)
            (*         printf "Aborted expression, restarting with next line.\n" *)
      | Expander.IllegalExpression (expr, msg) ->
          signalError (sprintf "Could not translate expression: %s\nexpr: %s\n" msg (Ast2.expression2string expr))
      | Lang.CouldNotParseType descr ->
          signalError (sprintf "Unknown type: %s\n" descr)
      | FailedToEvaluateLLVMCode (llvmCode, errorMsg) ->
          signalError (sprintf "Could not evaluate LLVM code: %s\n%s\n" errorMsg llvmCode)
    end
  with
    | CatchedError msg ->
        printf "%s" msg;
        onError()

let compileExpr translateF bindings sexpr = 
  let newBindings, simpleforms = translateF bindings sexpr in
  let llvmCodes = List.map Genllvm.gencodeTL simpleforms in
  let llvmCode = combine "\n" llvmCodes in
  newBindings, simpleforms, llvmCode

let rec compile ~readExpr ?(beforeCompilingExpr = fun (_:Ast2.sexpr) -> ()) ~onSuccess bindings =
  catchingErrorsDo
    (fun () -> begin
       match readExpr bindings with
         | Some expr ->
             let () = beforeCompilingExpr expr in
             let newBindings, simpleforms, llvmCode = compileExpr Expander.translateTL bindings expr in
             let () = onSuccess expr bindings newBindings simpleforms llvmCode in
             compile ~readExpr ~beforeCompilingExpr ~onSuccess newBindings
         | None ->
             bindings
     end)
    ~onError:(fun () -> compile ~readExpr ~beforeCompilingExpr ~onSuccess bindings)
  
let loadPrelude ?(processExpr = fun _ _ _ _ _ -> ()) ~dir =
  let rec parse parseF (lexbuf :Lexing.lexbuf) codeAccum =
    try
      let expr = parseF lexbuf in
      parse parseF lexbuf (codeAccum @ [expr])
    with
      | Lexer2.Eof | Sexprlexer.Eof -> codeAccum
  in
  
  let llvmPreludeFile = dir ^ "stdlib.ll" in
(*   printf "Loading LLVM prelude from %s\n" llvmPreludeFile; flush stdout; *)
  Zompvm.loadLLVMFile llvmPreludeFile;
  
  let zompPreludeFile = dir ^ "stdlib.zomp" in
(*   printf "Loading Zomp prelude from %s\n" zompPreludeFile; flush stdout; *)
  let lexbuf = Lexing.from_channel (open_in zompPreludeFile) in
  let parseF = Sexprparser.main Sexprlexer.token in
  let exprs = ref (parse parseF lexbuf []) in
  let readExpr _ =
    match !exprs with
      | next :: rem ->
          exprs := rem;
          Some next
      | [] -> None
  in
  let newBindings =
    compile
      ~readExpr
      ~onSuccess:(fun expr oldBindings newBindings simpleforms llvmCode ->
                    Zompvm.evalLLVMCode oldBindings simpleforms llvmCode;
                    processExpr expr oldBindings newBindings simpleforms llvmCode)
(*       ~onSuccess:(fun expr oldBindings newBindings simpleforms llvmCode -> *)
(*                     Zompvm.evalLLVMCode oldBindings simpleforms llvmCode *)
(*                  ) *)
      Genllvm.defaultBindings
  in
  newBindings
  (*     let newBindings = *)
  (*       List.fold_left *)
  (*         (fun bindings expr -> *)
  (*            let newBindings, simpleforms, llvmCode = compileExpr Expander.translateTL bindings expr in *)
  (*            Zompvm.evalLLVMCode bindings simpleforms llvmCode; *)
  (*            newBindings) *)
  (*         Genllvm.defaultBindings *)
  (*         exprs *)
  (*     in *)
