(*
 * Utilities which depend on parser and macro expander
 *)

open Printf
open Common
open Basics
open Parseutils

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
  end

type env = Expander.tlenv
let createEnv initialBindings = Expander.createEnv initialBindings
let bindings env = Expander.bindings env

let compileExpr env expr =
  let result =
    catchingErrorsDo (fun () -> Expander.translate env expr)
      ~onErrors:(fun diagnostics -> Result.fail ~results:[] ~diagnostics)
  in
  let llvmCode = Common.combine "\n" $ List.map Genllvm.gencodeTL result.Result.results in
  result, llvmCode

let compileNew env exprs emitBackendCode fileName =
  begin
    let exprs = List.map (fixFileName fileName) exprs in
    let hadError = ref false in
    let translateExpr expr =
      let oldBindings = bindings env in
      let { Result.flag; diagnostics; results }, llvmCode = compileExpr env expr in
      Zompvm.evalLLVMCode oldBindings results llvmCode;
      emitBackendCode llvmCode;
      if flag = Result.Fail then
        hadError := true;
      results, diagnostics
    in
    let formsNested, diagnosticsNested = List.split $ List.map translateExpr exprs in
    let diagnostics = List.flatten diagnosticsNested in
    let results = List.flatten formsNested in
    Result.make
      (if !hadError then Result.Fail else Result.Success)
      ~diagnostics ~results
  end

let compileFromStream env ~source ~emitBackendCode ~fileName =
  match collectTimingInfo "parsing" $ fun () -> Parseutils.parseIExprs ~fileName source with
    | Error error ->
      Result.make Result.Fail ~diagnostics:[error] ~results:[]
    | Exprs exprs ->
      compileNew env exprs emitBackendCode fileName

let loadPrelude env ?(emitBackendCode = fun _ -> ()) ?(appendSource = "") dir =
  let dir = if dir.[String.length dir - 1] = '/' then dir else dir ^ "/" in
  let llvmRuntimeFile = dir ^ "runtime.ll" in
  (collectTimingInfo "loading .ll file"
     (fun () -> Zompvm.loadLLVMFile llvmRuntimeFile));

  let preludeBaseName = "prelude" in
  let zompPreludeFile =
    Common.canonicalFileName $ Common.absolutePath (dir ^ preludeBaseName ^ ".zomp")
  in
  let source = Common.readFile zompPreludeFile ^ appendSource in
  match collectTimingInfo "parsing" $ fun () -> Parseutils.parseIExprs ~fileName:zompPreludeFile source with
    | Error error ->
      Result.make Result.Fail ~diagnostics:[error] ~results:[]
    | Exprs exprs ->
      List.iter Ast2.assertHasLocation exprs;
      let result = compileNew env exprs emitBackendCode zompPreludeFile in
      Result.replaceResults result (fun _ -> [])

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

