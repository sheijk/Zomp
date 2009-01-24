
open Printf
open Common
open Lang

include Machine

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
  tryApplyToAll
    removeFunctionBody
    redefinedFunctions
    ~onError:(fun msg -> raiseFailedToEvaluateLLVMCode llvmCode ("Could not remove function body: " ^ msg));
  if not (Machine.zompSendCode llvmCode targetModuleName) then
    raiseFailedToEvaluateLLVMCode llvmCode "Could not evaluate";
  tryApplyToAll
    recompileAndRelinkFunction
    redefinedFunctions
    ~onError:(fun msg -> raiseFailedToEvaluateLLVMCode llvmCode ("Could not recompile and relink function: " ^ msg))

let evalLLVMCode ?(targetModule = Runtime) bindings simpleforms llvmCode :unit =
  let isDefinedFunction func =
    match func.impl with
      | None -> false
      | Some _ ->
          match Bindings.lookup bindings func.fname with
            | Bindings.FuncSymbol _ -> true
            | _ -> false
  in
  let redefinedFunctions = List.fold_left
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

(** Callbacks which can be called from Zomp *)
module MeshCache : sig
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



