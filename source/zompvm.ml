
open Printf
open Common
open Lang

include Machine

exception FailedToEvaluateLLVMCode of Basics.location * string * string

let init() = Machine.zompInit()
let shutdown() = Machine.zompShutdown()

let setVerifyCode on = Machine.zompVerifyCode on
let verifyCode() = Machine.zompDoesVerifyCode()
let autoOptimizeFunctions() = Machine.zompOptimizeFunction()
let setAutoOptimizeFunctions on = Machine.zompSetOptimizeFunction on
let printInvalidLlvmCode() = Machine.zompPrintInvalidLlvmCode()
let setPrintInvalidLlvmCode on = Machine.zompSetPrintInvalidLlvmCode on

let optimizeCode() = Machine.zompOptimizeFunctions()

let buildInfo() = Machine.zompBuildInfo()
let isDebugBuild() = Machine.zompIsDebugBuild()
let printTimingStats() = Machine.zompPrintTimingStats()
let writeLLVMCodeToFile name = Machine.zompWriteLLVMCodeToFile name
let printModuleCode() = Machine.zompPrintModuleCode()
let printFunctionCode name = Machine.zompPrintFunctionCode name
let printGlobalVarCode name = Machine.zompPrintGlobalVarCode name

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
  type t = cptr

  let addr ast = ast

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

  let simple name = Machine.zompSimpleAst name
  let isNull ast = Machine.zompAstIsNull ast
  let addChild ast child = Machine.zompAddChild ast child
end

module Call =
struct
  let reset() = Machine.zompResetArgs()
  let addPointerArg ptr = Machine.zompAddPointerArg ~ptr
  let addIntArg arg = Machine.zompAddIntArg ~arg

  let void ~name = Machine.zompRunFunction ~name
  let string ~name = Machine.zompRunFunctionStringWithArgs ~name
  let int ~name = Machine.zompRunFunctionIntWithArgs ~name
  let pointer ~name = Machine.zompRunFunctionPointerWithArgs ~name
  let bool ~name = Machine.zompRunFunctionBool ~name
end

module Macros =
struct
  type func = cptr

  let resetArgs() = Machine.zompResetMacroArgs()
  let addArg ast = Machine.zompAddMacroArg ast
  let call f = Machine.zompCallMacro f

  let addressOfMacroFunction ~name = Machine.zompAddressOfMacroFunction ~name
end

module Remote =
struct
  let connect ~uri = Machine.zompConnectToRemoteVM uri
  let disconnect() = Machine.zompDisconnectRemoteVM()

  let send ~uri = Machine.zompSendToRemoteVM uri
end

let raiseFailedToEvaluateLLVMCode loc llvmCode errorMessage =
  raise (FailedToEvaluateLLVMCode (loc, llvmCode, errorMessage))

(** Llvm textual IR. *)
type code = string
let codeFromLlvm llvmCode = llvmCode
let codeToString llvmCode = llvmCode

let removeFunctionBodies redefinedFunctions =
  let removeFunctionBody name = ignore (Machine.zompRemoveFunctionBody name) in
  List.iter removeFunctionBody redefinedFunctions

let relinkFunctions redefinedFunctions =
  let tryApplyToAll ~onError f list =
    List.iter
      (fun obj -> if not( f obj ) then onError obj)
      list
  in
  let recompileAndRelinkFunction name = Machine.zompRecompileAndRelinkFunction name in
  collectTimingInfo "recompile and relink functions"
    (fun () ->
      tryApplyToAll
        recompileAndRelinkFunction
        redefinedFunctions
        ~onError:(fun msg ->
                  raiseFailedToEvaluateLLVMCode
                    Basics.fakeLocation
                    ""
                    ("could not recompile and relink function: " ^ msg)))

let codeHandlers : (code -> unit) list ref = ref []

let registerCodeHandler handler =
  if not (List.mem handler !codeHandlers) then
    codeHandlers := handler :: !codeHandlers

let unregisterCodeHandler handler =
  codeHandlers := List.filter (fun h -> not (h = handler)) !codeHandlers

type phase = CompilationPhase | CompilationAndRuntimePhase

let evalCode phase llvmCode =
  if phase = CompilationAndRuntimePhase then
    List.iter (fun handler -> handler llvmCode) !codeHandlers;
  collectTimingInfo "send code"
    (fun () ->
      if not (Machine.zompSendCode llvmCode "") then begin
          raiseFailedToEvaluateLLVMCode Basics.fakeLocation llvmCode "could not evaluate"
        end)

let loadLLVMFile filename =
  try
    let content = readFile filename in
    if not( Machine.zompSendCode content "" ) then begin
      let error = Serror.fromMsg (Some (Basics.location filename 0 None)) "failed to evaluate LLVM code" in
      eprintf "%s\n" @@ Serror.toString error;
    end
  with Sys_error message ->
    let error = Serror.fromMsg (Some (Basics.location filename 0 None)) "could not load file" in
    eprintf "%s\n" @@ Serror.toString error

module DllHandle =
struct
  type t = cptr
  let addr handle = handle
end

let loadLib name = Machine.zompLoadLib name

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

