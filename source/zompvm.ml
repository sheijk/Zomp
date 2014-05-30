
open Printf
open Common
open Lang

include Machine

let flushStreams() =
  flush stdout;
  flush stderr;
  Machine.zompFlushStreams()

module StatisticsBackend : sig end = struct
  let registerCounter ~sectionName ~name ~fractionalDigits ~typ ~id =
    let f =
      match typ with
        | Statistics.Int -> Stats.statsCreateCamlIntCounter
        | Statistics.Float -> Stats.statsCreateCamlFloatCounter
    in
    f sectionName name fractionalDigits id
    
  let () =
    Statistics.setImplementation
      Stats.statsCreateNamedSection
      registerCounter
end

let zompvmSection = Statistics.createSection "zompvm"

(** Utilities to convert between Ast2.sexpr and Zomp native AST *)
module NativeAst =
struct
  let totalNativeAstsConverted = ref 0
  let nativeAstsConvertedSeconds = ref 0.0
  let totalOCamlAstsConverted = ref 0
  let ocamlAstsConvertedSeconds = ref 0.0

  let () =
    let addCounter name f =
      Statistics.createIntCounter zompvmSection name 0 f
    in
    let addCounterF name f =
      Statistics.createFloatCounter zompvmSection name 3 f
    in
    addCounter "total ASTs converted" (fun () -> !totalOCamlAstsConverted + !totalNativeAstsConverted);
    addCounter "OCaml ASTs from native converted" (Ref.getter totalOCamlAstsConverted);
    addCounter "native ASTs from OCaml converted" (Ref.getter totalNativeAstsConverted);
    addCounterF "converting ASTs (s)" (fun () -> !nativeAstsConvertedSeconds +. !ocamlAstsConvertedSeconds);
    addCounterF "converting native ASTs from OCaml (s)" (Ref.getter nativeAstsConvertedSeconds);
    addCounterF "converting OCaml ASTs from native (s)" (Ref.getter ocamlAstsConvertedSeconds);
    ()

  let isValidId name =
    foldString
      name (fun wasValid chr -> wasValid && Char.code chr < 128) true

  let rec extractSExprFromNativeAst astAddress =
    incr totalOCamlAstsConverted;
    if zompAstIsNull astAddress then
      Ast2.idExprLoc Basics.fakeLocation "error, macro returned NULL"
    else
      let name =
        let extracted = Machine.zompAstId astAddress in
        if isValidId extracted then extracted
        else sprintf "compiler:error:invalidId '%s'" extracted
      in
      let childCount = Machine.zompAstChildCount astAddress in
      let childs =
        let rec getChilds num =
          if num < childCount then
            let childAddress = Machine.zompAstChild astAddress num in
            let child = extractSExprFromNativeAst childAddress in
            child :: getChilds (num+1)
          else
            []
        in
        getChilds 0
      in
      let location = Basics.location
        (zompAstFile astAddress)
        (zompAstLine astAddress)
        (Some (zompAstColumn astAddress))
      in
      if Basics.locationValid location then
        Ast2.exprLoc location name childs
      else
        Ast2.exprNoLoc name childs

  let extractSExprFromNativeAst astAddress =
    addTiming ocamlAstsConvertedSeconds (fun () -> extractSExprFromNativeAst astAddress)

  let applyLocation nativeId ast =
    zompSetAstLocation nativeId (Ast2.fileName ast) (Ast2.lineNumber ast) (Ast2.column ast);
    nativeId

  let rec buildNativeAst ast =
    incr totalNativeAstsConverted;
    match ast with
      | {Ast2.id = id; args = []} ->
        applyLocation (zompSimpleAst id) ast
      | _ ->
        let childs = List.map buildNativeAst ast.Ast2.args in
        let nativeAst = zompSimpleAst ast.Ast2.id in
        List.iter (fun child -> zompAddChild ~parent:nativeAst ~child) childs;
        applyLocation nativeAst ast

  let buildNativeAst ast =
    addTiming nativeAstsConvertedSeconds (fun () -> buildNativeAst ast)
end

let raiseFailedToEvaluateLLVMCode llvmCode errorMessage = raise (FailedToEvaluateLLVMCode (llvmCode, errorMessage))

let evalLLVMCodeB redefinedFunctions llvmCode :unit =
  let tryApplyToAll ~onError f list =
    List.iter
      (fun obj -> if not( f obj ) then onError obj)
      list
  in
  let removeFunctionBody name = ignore (Machine.zompRemoveFunctionBody name) in
  let recompileAndRelinkFunction name = Machine.zompRecompileAndRelinkFunction name in
  List.iter removeFunctionBody redefinedFunctions;
  collectTimingInfo "send code"
    (fun () ->
       if not (Machine.zompSendCode llvmCode "") then
         raiseFailedToEvaluateLLVMCode llvmCode "could not evaluate");
  collectTimingInfo "recompile and relink functions"
    (fun () ->
       tryApplyToAll
         recompileAndRelinkFunction
         redefinedFunctions
         ~onError:(fun msg -> raiseFailedToEvaluateLLVMCode llvmCode ("could not recompile and relink function: " ^ msg)))

let evalLLVMCode simpleforms llvmCode :unit =
  flushStreams();
  let isDefinedFunction func =
    match func.impl with
      | None -> false
      | Some _ ->
        Machine.zompRemoveFunctionBody func.fname
  in
  let redefinedFunctions = Common.mapFilter (function
    | `DefineFunc func when isDefinedFunction func -> Some func.fname
    | _ -> None) simpleforms
  in
  evalLLVMCodeB redefinedFunctions llvmCode

let loadLLVMFile filename =
  try
    let content = readFile filename in
    if not( Machine.zompSendCode content "" ) then
      eprintf "could not eval llvm code from file %s\n" filename
  with
      Sys_error message ->
        eprintf "could not load file %s: %s\n" filename message

let currentBindings :Bindings.t ref = ref Bindings.defaultBindings

let isInteractiveFlag = ref false
let isInteractive() = !isInteractiveFlag
let setIsInteractive on = isInteractiveFlag := on

let traceMacroExpansionOn = ref false

(** Callbacks which can be called from Zomp. The C++ part of this is in zompvm_caml.cpp *)
module Callbacks : sig
end = struct

  let isBound name =
    match Bindings.lookup !currentBindings name with
      | Bindings.UndefinedSymbol -> false
      | _ -> true

  (* also defined in zompvm_impl.cpp *)
  let symbolUndefined = 0
  and symbolVar = 1
  and symbolFunc = 2
  and symbolMacro = 3
  and symbolTypedef = 4
  and symbolLabel = 5

  let lookup name =
    match Bindings.lookup !currentBindings name with
      | Bindings.UndefinedSymbol -> symbolUndefined
      | Bindings.VarSymbol _ -> symbolVar
      | Bindings.FuncSymbol _ -> symbolFunc
      | Bindings.MacroSymbol _ -> symbolMacro
      | Bindings.TypedefSymbol _ -> symbolTypedef
      | Bindings.LabelSymbol _ -> symbolLabel

  let hello_callback () =
    printf "Hello, callback!\n";
    flush stdout

  let printString str =
    printf "Printing string from C: %s\n" str;
    flush stdout

  let getTrue () = true

  let () =
    Callback.register "helloCallback" hello_callback;
    Callback.register "printString" printString;
    Callback.register "getTrue" getTrue;

    Callback.register "isBound" isBound;
    Callback.register "lookup" lookup;
    ()

end

let isNullPtr handle = zompAstIsNull handle

