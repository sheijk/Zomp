(*
 * Utilities which depend on parser and macro expander
 *)

open Printf
open Common
open Basics
open Parseutils

let compileExpr = Expander.compileExpr

let compileNew env exprs fileName =
  let exprs = List.map (fixFileName fileName) exprs in
  Expander.translateMulti env exprs

let compileFromStream env ~source ~fileName =
  let parse ~fileName source = collectTimingInfo "parsing" $ 
    fun () -> Parseutils.parseIExprs ~fileName source
  in
  match parse ~fileName source with
    | Error error ->
      Result.make Result.Fail ~diagnostics:[error] ~results:[]
    | Exprs exprs ->
      compileNew env exprs fileName

let loadPrelude env ?(appendSource = "") dir =
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
      let result = compileNew env exprs zompPreludeFile in
      Result.replaceResults result (fun _ -> [])

let writeSymbolsToStream bindings stream =
  fprintf stream "Symbol table\n";

  let printSymbol (name, info) =
    fprintf stream "%s =" name;
    let location = Bindings.location info in
    let doc =
      match Bindings.symbol info with
        | Bindings.VarSymbol var ->
          sprintf "var of type %s" (Types.typeName var.Lang.typ)
        | Bindings.FuncSymbol func ->
          let argToString (name, typ) =
            sprintf "%s %s" (Types.typeName typ) name
          in
          let args = List.map argToString func.Lang.fargs in
          let argString = Common.combine ", " args in
          sprintf "%s(%s)" (Types.typeName func.Lang.rettype) argString
        | Bindings.MacroSymbol macro ->
          sprintf "%s" macro.Lang.mdocstring
        | Bindings.LabelSymbol label ->
          sprintf "label %s" label.Lang.lname
        | Bindings.TypedefSymbol typ ->
          sprintf "type %s" (Types.typeDescr typ)
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

