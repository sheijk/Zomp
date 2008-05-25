
open Common
open Printf
  
let rec parse parseF lexbuf bindings codeAccum =
  try
    let expr = parseF lexbuf in
    let newBindings, simpleforms = Expander.translateTL bindings expr in
    parse parseF lexbuf newBindings (codeAccum @ simpleforms)
  with
    | Sexprlexer.Eof | Indentlexer.Eof -> bindings, codeAccum

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
      | Expander.IllegalExpression (expr, msg) ->
          signalError (sprintf "Could not translate expression: %s\nexpr: %s\n" msg (Ast2.expression2string expr))
      | Lang.CouldNotParseType descr ->
          signalError (sprintf "Unknown type: %s\n" descr)
      | Genllvm.CodeGenError msg ->
          signalError (sprintf "Codegen failed: %s\n" msg)
      | FailedToEvaluateLLVMCode (llvmCode, errorMsg) ->
          signalError (sprintf "Could not evaluate LLVM code: %s\n%s\n" errorMsg llvmCode)
      | Failure msg ->
          signalError (sprintf "Internal error: Failure(%s)\n" msg)
    end
  with
    | CatchedError msg ->
(*         printf "%s" msg; *)
        onError msg
          
let compileExpr translateF bindings sexpr =
  let newBindings, simpleforms =
    collectTimingInfo "generating ast"
      (fun () -> translateF bindings sexpr)
  in
  let llvmCodes =
    collectTimingInfo "codegen"
      (fun () -> List.map Genllvm.gencodeTL simpleforms)
  in
  let llvmCode = combine "\n" llvmCodes in
  newBindings, simpleforms, llvmCode

let rec compile
    ~readExpr
    ?(beforeCompilingExpr = fun (_:Ast2.sexpr) -> ())
    ~onSuccess
    ?(onError = fun (_:string) -> ())
    bindings
    =
  catchingErrorsDo
    (fun () -> begin
       match readExpr bindings with
         | Some expr ->
             let () = beforeCompilingExpr expr in
             let newBindings, simpleforms, llvmCode = compileExpr Expander.translateTL bindings expr in
             let () = onSuccess expr bindings newBindings simpleforms llvmCode in
             compile ~readExpr ~beforeCompilingExpr ~onSuccess ~onError newBindings
         | None ->
             bindings
     end)
    ~onError:(fun msg ->
                printf "%s" msg;
                let () = onError msg in
                compile ~readExpr ~beforeCompilingExpr ~onSuccess ~onError bindings)

let loadPrelude ?(processExpr = fun _ _ _ _ _ -> ()) ~dir :Bindings.t =
  let rec parse parseF (lexbuf :Lexing.lexbuf) codeAccum =
    try
      let expr = parseF lexbuf in
      parse parseF lexbuf (codeAccum @ [expr])
    with
        Sexprlexer.Eof | Indentlexer.Eof -> codeAccum
  in

  let dir = if dir.[String.length dir - 1] = '/' then dir else dir ^ "/" in
  let llvmPreludeFile = dir ^ "stdlib.ll" in
  (collectTimingInfo "loading .ll file"
     (fun () -> Zompvm.loadLLVMFile llvmPreludeFile));
  
  let zompPreludeFile = dir ^ "stdlib.zomp" in
  let lexbuf = Lexing.from_channel (open_in zompPreludeFile) in
  let parseF = Sexprparser.main Sexprlexer.token in
  let exprs =
    collectTimingInfo "parsing"
      (fun () -> ref (parse parseF lexbuf []))
  in
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
      Genllvm.defaultBindings
  in
  newBindings

    
