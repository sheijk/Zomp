
open Printf
open Common
open Lang

include Machine
  
exception FailedToEvaluateLLVMCode of string * string

let raiseFailedToEvaluateLLVMCode llvmCode errorMessage = raise (FailedToEvaluateLLVMCode (llvmCode, errorMessage))

let compileExpr translateF bindings sexpr = 
  let newBindings, simpleforms = translateF bindings sexpr in
  let llvmCodes = List.map Genllvm.gencodeTL simpleforms in
  let llvmCode = combine "\n" llvmCodes in
  newBindings, simpleforms, llvmCode

type targetModule = Runtime | Compiletime
    
let evalLLVMCode ?(targetModule = Runtime) bindings simpleforms llvmCode :unit =
  let targetModuleName = match targetModule with Runtime -> "" | Compiletime -> "compiletime" in
  let isDefinedFunction func =
    match func.impl, Bindings.lookup bindings func.fname with
      | Some _, Bindings.FuncSymbol _ -> true
      | _, _ -> false
  in
  let redefinedFunctions = List.fold_left
    (fun redefinedFunctions toplevelForm ->
       match toplevelForm with
         | `DefineFunc func when isDefinedFunction func -> func.fname :: redefinedFunctions
         | _ -> redefinedFunctions )
    []
    simpleforms
  in
  let tryApplyToAll ~onError f list =
    List.iter
      (fun obj -> if not( f obj ) then onError obj)
      list
  in
  let removeFunctionBody name = Machine.zompRemoveFunctionBody name in
  let recompileAndRelinkFunction name = Machine.zompRecompileAndRelinkFunction name in
  tryApplyToAll removeFunctionBody redefinedFunctions ~onError:(raiseFailedToEvaluateLLVMCode llvmCode);
  if not (Machine.zompSendCode llvmCode targetModuleName) then
    raiseFailedToEvaluateLLVMCode llvmCode "Could not evaluate";
  tryApplyToAll recompileAndRelinkFunction redefinedFunctions ~onError:(raiseFailedToEvaluateLLVMCode llvmCode)
  

  
