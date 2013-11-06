(*
 * Utilities which depend on parser and macro expander
 *)

open Printf
open Common
open Basics
open Parseutils

exception CatchedError of Serror.t list
let signalErrors errors = raise (CatchedError errors)

exception CouldNotParse of Serror.t

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

let catchingErrorsDo f ~onErrors =
  let onErrorMsg msg = onErrors [Serror.fromMsg None msg] in
  begin
    try
      f()
    with
      | Expander.IllegalExpression (expr, errors) ->
        onErrors errors
      | Lang.CouldNotParseType descr ->
        onErrorMsg $ sprintf "unknown type: %s\n" descr
      | Genllvm.CodeGenError msg ->
        onErrorMsg $ sprintf "codegen failed: %s\n" msg
      | FailedToEvaluateLLVMCode (llvmCode, errorMsg) ->
        onErrorMsg $ sprintf "could not evaluate LLVM code: %s\n%s\n" errorMsg llvmCode
      | Failure msg ->
        onErrorMsg $ sprintf "internal error: exception Failure(%s)\n" msg
      | CatchedError errors ->
        onErrors errors
      | CouldNotParse error ->
        onErrors [error]
  end

let rec compile
    ~readExpr
    ?(beforeCompilingExpr = fun (_:Ast2.sexpr) -> ())
    ~onSuccess
    ~onErrors
    bindings
    =
  let continue = compile ~readExpr ~beforeCompilingExpr ~onSuccess ~onErrors in
  catchingErrorsDo
    (fun () -> begin
       match readExpr bindings with
         | Some expr ->
             let () = beforeCompilingExpr expr in
             let newBindings, simpleforms, llvmCode = compileExpr translateTLNoError bindings expr in
             let () = onSuccess expr bindings newBindings simpleforms llvmCode in
             continue newBindings
         | None ->
             bindings
     end)
    ~onErrors:(fun errors ->
      let () = onErrors errors in
      continue bindings)

let compileCode bindings input outStream fileName =
  (** TODO: return error(s) instead of printing them *)
  let printError exn =
    match exn with
      | CouldNotParse error ->
        eprintf "%s\n" $ Serror.toString error
      | CatchedError errors ->
        List.iter (fun error -> eprintf "%s\n" (Serror.toString error)) errors;
        eprintf "  (via exception CatchedError)\n"
      | unknownError ->
        eprintf "error: unknown error: %s\n" (Printexc.to_string unknownError);
        raise unknownError
  in
  let parseAndCompile parseF =
    let exprs =
      collectTimingInfo "parsing"
        (fun () ->
           match parseF ~fileName input with
             | Parseutils.Exprs exprs ->
               List.map (fixFileName fileName) exprs
             | Parseutils.Error error ->
               raise (CouldNotParse error))
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
                         output_string outStream llvmCode)
           ~onErrors:signalErrors
           bindings)
  in
  tryAllCollectingErrors
    [lazy (parseAndCompile Parseutils.parseIExprs)]
    ~onSuccess:(fun finalBindings -> Some finalBindings)
    ~ifAllFailed:(fun exceptions ->
                    List.iter printError exceptions;
                    None)

let loadPrelude ?(processExpr = fun _ _ _ _ _ -> ()) ?(appendSource = "") dir :Bindings.t =
  let dir = if dir.[String.length dir - 1] = '/' then dir else dir ^ "/" in
  let llvmRuntimeFile = dir ^ "runtime.ll" in
  (collectTimingInfo "loading .ll file"
     (fun () -> Zompvm.loadLLVMFile llvmRuntimeFile));

  let preludeBaseName = "prelude" in
  let zompPreludeFile = Common.canonicalFileName
    (Common.absolutePath (dir ^ preludeBaseName ^ ".zomp")) in
  let source = Common.readFile zompPreludeFile ^ appendSource in
  let exprs =
    collectTimingInfo "parsing"
      (fun () ->
        ref (match Parseutils.parseIExprs ~fileName:zompPreludeFile source with
          | Exprs exprs ->
            List.iter Ast2.assertHasLocation exprs;
            exprs
          | Error pe -> raise (CouldNotParse pe)))
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
      ~onErrors:signalErrors
      Genllvm.defaultBindings
  in
  newBindings

let writeSymbolsToStream bindings stream =
  fprintf stream "Symbol table\n";

  let printSymbol (name, info) =
    let module Typesystem = Typesystems.Zomp in
    fprintf stream "%s =" name;
    let location = Bindings.location info in
    let doc =
      match Bindings.symbol info with
        | Bindings.VarSymbol var ->
          sprintf "var of type %s" (Typesystem.typeName var.Lang.typ)
        | Bindings.FuncSymbol func ->
          let argToString (name, typ) =
            sprintf "%s %s" (Typesystem.typeName typ) name
          in
          let args = List.map argToString func.Lang.fargs in
          let argString = Common.combine ", " args in
          sprintf "%s(%s)" (Typesystem.typeName func.Lang.rettype) argString
        | Bindings.MacroSymbol macro ->
          sprintf "%s" macro.Lang.mdocstring
        | Bindings.LabelSymbol label ->
          sprintf "label %s" label.Lang.lname
        | Bindings.TypedefSymbol typ ->
          sprintf "type %s" (Typesystem.typeDescr typ)
        | Bindings.UndefinedSymbol ->
          sprintf "undefined"
    in
    begin match location with
      | Some location -> fprintf stream "%s @%s" doc (Basics.locationToString location)
      | None -> fprintf stream "%s" doc
    end;
    fprintf stream "\n"
  in
  Bindings.iterInfo printSymbol bindings;

  let printBuiltinDoc name params = fprintf stream "%s =%s\n" name params in
  Expander.foreachBaseInstructionDoc printBuiltinDoc;
  (** zomp.el does not distinguish between toplevel and regular expressions *)
  Expander.foreachToplevelBaseInstructionDoc printBuiltinDoc

(**
 * Writes a very primitive symbol table to the given file. Deletes file if it
 * existed
 *)
let writeSymbols bindings fileName =
  try begin
    if Sys.file_exists fileName then
      Sys.remove fileName;
    let stream = open_out fileName in
    try
      writeSymbolsToStream bindings stream;
      close_out stream;
      true
    with exn ->
      close_out stream;
      raise exn
  end with Sys_error _ ->
    false

