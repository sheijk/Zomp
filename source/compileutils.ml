(*
 * Utilities which depend on parser and macro expander
 *)

open Printf
open Common
open Basics
open Parseutils

exception CatchedError of Expander.SError.t list
let signalErrors errors = raise (CatchedError errors)

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
  let onErrorMsg msg = onErrors [Expander.SError.fromMsg None msg] in
  begin
    try
      f()
    with
      | Expander.IllegalExpression (expr, errors) ->
        onErrors errors
      | Lang.CouldNotParseType descr ->
        onErrorMsg $ sprintf "Unknown type: %s\n" descr
      | Genllvm.CodeGenError msg ->
        onErrorMsg $ sprintf "Codegen failed: %s\n" msg
      | FailedToEvaluateLLVMCode (llvmCode, errorMsg) ->
        onErrorMsg $ sprintf "Could not evaluate LLVM code: %s\n%s\n" errorMsg llvmCode
      | Failure msg ->
        onErrorMsg $ sprintf "Internal error: Failure(%s)\n" msg
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

exception CouldNotParse of parseError

let compileCode bindings input outstream fileName =
  (** TODO: return error(s) instead of printing them *)
  let printError exn =
    match exn with
      | CouldNotParse err ->
        let location = (match err.location with Some loc -> loc | None -> fakeLocation) in
        eprintf "%s\n" $ formatError location err.reason
      | CatchedError errors ->
        List.iter (fun error -> eprintf "%s\n" (Expander.SError.toString error)) errors;
        eprintf "  (via exception CatchedError)\n"
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
               List.map (fixFileName fileName) exprs
             | Parseutils.Error error ->
                 let errorWithCorrectFile = {
                   error with
                     location = match error.location with
                       | Some l -> Some { l with fileName = fileName }
                       | None -> error.location }
                 in
                 raise (CouldNotParse errorWithCorrectFile))
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
  let zompPreludeFile = Common.absolutePath (dir ^ preludeBaseName ^ ".zomp") in
  let source = Common.readFile zompPreludeFile ^ appendSource in
  let exprs =
    collectTimingInfo "parsing"
      (fun () ->
        ref (match Parseutils.parseIExprs source with
          | Exprs e -> List.map (fixFileName zompPreludeFile) e
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


(**
 * Writes a very primitive symbol table to the given file. Deletes file if it
 * existed
 *)
let writeSymbols fileName bindings =
  try begin
    let module Typesystem = Typesystems.Zomp in
    if Sys.file_exists fileName then
      Sys.remove fileName;
    let stream = open_out fileName in
    try
      fprintf stream "Symbol table\n";

      let printSymbol (name, symbol) =
        fprintf stream "%s =" name;
        begin match symbol with
          | Bindings.VarSymbol var ->
            fprintf stream "var of type %s %s"
              (Typesystem.typeName var.Lang.typ)
              (match var.Lang.vlocation with
                | None -> "@?"
                | Some loc ->
                  "@" ^ Basics.locationToString loc)
          | Bindings.FuncSymbol func ->
            let argToString (name, typ) =
              sprintf "%s %s" (Typesystem.typeName typ) name
            in
            let args = List.map argToString func.Lang.fargs in
            let argString = Common.combine ", " args in
            fprintf stream "%s(%s)"
              (Typesystem.typeName func.Lang.rettype)
              argString;
          | Bindings.MacroSymbol macro ->
            fprintf stream "%s" macro.Lang.mdocstring;
          | Bindings.LabelSymbol label ->
            fprintf stream "label %s" label.Lang.lname;
          | Bindings.TypedefSymbol typ ->
            fprintf stream "type %s" (Typesystem.typeDescr typ)
          | Bindings.UndefinedSymbol ->
            fprintf stream "undefined"
        end;
        fprintf stream "\n"
      in
      Bindings.iter printSymbol bindings;

      let printBuiltinDoc name params = fprintf stream "%s =%s\n" name params in
      Expander.foreachBaseInstructionDoc printBuiltinDoc;
      (** zomp.el does not distinguish between toplevel and regular expressions *)
      Expander.foreachToplevelBaseInstructionDoc printBuiltinDoc;

      close_out stream;
      true
    with exn ->
      close_out stream;
      raise exn
 end with Sys_error _ ->
   false

