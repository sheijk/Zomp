
open Printf
open Common
open Lang

include Machine

(** Utilities to convert between Ast2.sexpr and Zomp native AST *)
module NativeAst =
struct

  let isValidId name =
    foldString
      name (fun wasValid chr -> wasValid && Char.code chr < 128) true

  let rec extractSExprFromNativeAst astAddress =
    if zompAstIsNull astAddress then
      Ast2.idExpr "error, macro returned NULL"
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
      Ast2.expr name childs

  let rec buildNativeAst = function
    | {Ast2.id = id; args = []} ->
        zompSimpleAst id
    | ast ->
        let childs = List.map buildNativeAst ast.Ast2.args in
        let nativeAst = zompSimpleAst ast.Ast2.id in
        List.iter (fun child -> zompAddChild ~parent:nativeAst ~child) childs;
        nativeAst
end

(* exception FailedToEvaluateLLVMCode of string * string *)

let raiseFailedToEvaluateLLVMCode llvmCode errorMessage = raise (FailedToEvaluateLLVMCode (llvmCode, errorMessage))


type targetModule = Runtime | Compiletime


let evalLLVMCodeB ?(targetModule = Runtime) redefinedFunctions simpleforms llvmCode :unit =
  let targetModuleName = match targetModule with Runtime -> "" | Compiletime -> "compiletime" in
  let tryApplyToAll ~onError f list =
    List.iter
      (fun obj -> if not( f obj ) then onError obj)
      list
  in
  let removeFunctionBody name = Machine.zompRemoveFunctionBody name in
  let recompileAndRelinkFunction name = Machine.zompRecompileAndRelinkFunction name in
  (* let removeFunctionBody = sampleFunc1 "removeFunctionBody" removeFunctionBody in *)
  (* let recompileAndRelinkFunction = sampleFunc1 "recompileAndRelinkFunction" recompileAndRelinkFunction in *)
  collectTimingInfo "removeFunctionBodies"
    (fun () ->
       tryApplyToAll
         removeFunctionBody
         redefinedFunctions
         ~onError:(fun msg -> raiseFailedToEvaluateLLVMCode llvmCode ("Could not remove function body: " ^ msg)));
  collectTimingInfo "send code"
    (fun () ->
       if not (Machine.zompSendCode llvmCode targetModuleName) then
         raiseFailedToEvaluateLLVMCode llvmCode "Could not evaluate");
  collectTimingInfo "recompile and relink functions"
    (fun () ->
       tryApplyToAll
         recompileAndRelinkFunction
         redefinedFunctions
         ~onError:(fun msg -> raiseFailedToEvaluateLLVMCode llvmCode ("Could not recompile and relink function: " ^ msg)))

let evalLLVMCode ?(targetModule = Runtime) bindings simpleforms llvmCode :unit =
  let isDefinedFunction func =
    match func.impl with
      | None -> false
      | Some _ ->
          match Bindings.lookup bindings func.fname with
            | Bindings.FuncSymbol _ -> true
            | _ -> false
  in
  let redefinedFunctions =
    List.fold_left
    (fun redefinedFunctions toplevelForm ->
       match toplevelForm with
         | `DefineFunc func when isDefinedFunction func -> func.fname :: redefinedFunctions
         | _ -> redefinedFunctions )
    []
    simpleforms
  in
  evalLLVMCodeB ~targetModule redefinedFunctions simpleforms llvmCode

let loadLLVMFile filename =
  try
    let content = readFile filename in
    if not( Machine.zompSendCode content "" ) then
      eprintf "Could not eval llvm code from file %s\n" filename
  with
      Sys_error message ->
        eprintf "Could not load file %s: %s\n" filename message

let currentBindings :Bindings.t ref = ref (Bindings.addTypedef Bindings.defaultBindings "asdf" `Int8)

let isInteractiveFlag = ref false
let isInteractive() = !isInteractiveFlag
let setIsInteractive on = isInteractiveFlag := on

(** Callbacks which can be called from Zomp *)
module Callbacks : sig
end = struct

  let isBound name =
    match Bindings.lookup !currentBindings name with
      | Bindings.UndefinedSymbol -> false
      | _ -> true

  (* also defined in zompvm.cpp *)
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



