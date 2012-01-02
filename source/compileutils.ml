(*
 * Utilities which depend on parser and macro expander
 *)

open Common
open Printf
open Parseutils

exception CatchedError of string
let signalError msg = raise (CatchedError msg)

let translateTLNoError bindings expr =
  match Expander.translateTL bindings expr with
    | Expander.Result r -> r
    | Expander.Error errors -> failwith "unexpected failure in Expander.translateTL"

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

let rec parse parseF lexbuf bindings codeAccum =
  try
    let expr = parseF lexbuf in
    let newBindings, simpleforms = translateTLNoError bindings expr in
    parse parseF lexbuf newBindings (codeAccum @ simpleforms)
  with
    | Indentlexer.Eof -> bindings, codeAccum

let catchingErrorsDo f ~onError =
  try
    begin try
      f()
    with
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

let rec compile
    ~readExpr
    ?(beforeCompilingExpr = fun (_:Ast2.sexpr) -> ())
    ~onSuccess
    ~onError
    bindings
    =
  catchingErrorsDo
    (fun () -> begin
       match readExpr bindings with
         | Some expr ->
             let () = beforeCompilingExpr expr in
             let newBindings, simpleforms, llvmCode = compileExpr translateTLNoError bindings expr in
             let () = onSuccess expr bindings newBindings simpleforms llvmCode in
             compile ~readExpr ~beforeCompilingExpr ~onSuccess ~onError newBindings
         | None ->
             bindings
     end)
    ~onError:(fun msg ->
                let () = onError msg in
                compile ~readExpr ~beforeCompilingExpr ~onSuccess ~onError bindings)

exception CouldNotParse of string
let raiseCouldNotParse str = raise (CouldNotParse str)

exception CouldNotCompile of string
let raiseCouldNotCompile str = raise (CouldNotCompile str)

let compileCode bindings input outstream fileName =
  (** TODO: return error(s) instead of printing them *)
  let printError = function
    | CouldNotParse msg -> eprintf "error: %s" msg
    | CouldNotCompile msg -> eprintf "error: %s" msg
    | CatchedError msg -> eprintf "error: %s" msg
    | unknownError ->
        eprintf "error: Unknown error: %s\n" (Printexc.to_string unknownError);
        raise unknownError
  in
  let parseAndCompile parseF =
    let exprs =
      collectTimingInfo "parsing"
        (fun () ->
           match parseF input with
             | Parseutils.Exprs exprs ->
               let rec fixFileName expr =
                 let fixedArgs = List.map fixFileName expr.Ast2.args in
                 match expr.Ast2.location with
                   | None ->
                     { expr with Ast2.args = fixedArgs }
                   | Some loc ->
                     { expr with
                       Ast2.location = Some { loc with Ast2.fileName = fileName };
                       Ast2.args = fixedArgs }
               in
               List.map fixFileName exprs
             | Parseutils.Error error ->
                 let errorWithCorrectFile = {
                   error with
                     location = match error.location with
                       | Some l -> Some { l with Indentlexer.fileName = fileName }
                       | None -> error.location }
                 in
                 raiseCouldNotParse (parseErrorToString errorWithCorrectFile))
    in
    let leftExprs = ref exprs in
    let readExpr _ =
      match !leftExprs with
        | next :: rem ->
            leftExprs := rem;
            Some next
        | [] -> None
    in
    collectTimingInfo "compiling"
      (fun () ->
         compile
           ~readExpr
           ~onSuccess:(fun expr oldBindings newBindings simpleforms llvmCode ->
                         Zompvm.evalLLVMCode oldBindings simpleforms llvmCode;
                         output_string outstream llvmCode)
           ~onError:signalError
           bindings)
  in
  tryAllCollectingErrors
    [lazy (parseAndCompile Parseutils.parseIExprs)]
    ~onSuccess:(fun _ -> Some ())
    ~ifAllFailed:(fun exceptions ->
                    List.iter printError exceptions;
                    None)

let loadPrelude ?(processExpr = fun _ _ _ _ _ -> ()) ?(appendSource = "") dir :Bindings.t =
  let rec parse parseF (lexbuf :Lexing.lexbuf) codeAccum =
    try
      let expr = parseF lexbuf in
      parse parseF lexbuf (codeAccum @ [expr])
    with
        Indentlexer.Eof -> codeAccum
  in

  let dir = if dir.[String.length dir - 1] = '/' then dir else dir ^ "/" in
  let runtimeBaseName = "source/runtime" in
  let llvmRuntimeFile = dir ^ runtimeBaseName ^ ".ll" in
  (collectTimingInfo "loading .ll file"
     (fun () -> Zompvm.loadLLVMFile llvmRuntimeFile));

  let preludeBaseName = "prelude" in
  let zompPreludeFile = dir ^ preludeBaseName ^ ".zomp" in
  let source = Common.readFile zompPreludeFile ^ appendSource in
  let lexbuf = Lexing.from_string source in
  let lexstate = Indentlexer.lexbufFromString zompPreludeFile source in
  let lexFunc lexbuf =
    let r = Indentlexer.token lexstate in
    let loc = Indentlexer.locationOfLexstate lexstate in
    let start = lexbuf.Lexing.lex_start_p in
    lexbuf.Lexing.lex_start_p <- { start with Lexing.pos_lnum = loc.Indentlexer.line };
    r
  in
  let parseF = Newparser.main lexFunc in
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
      ~onError:(fun msg -> signalError msg)
      Genllvm.defaultBindings
  in
  newBindings

