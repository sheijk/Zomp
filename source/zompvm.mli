type double = float
type cptr = Machine.cptr
external zompInit : unit -> bool = "ml_zompInit"
external zompShutdown : unit -> unit = "ml_zompShutdown"
external zompRequestedPause : unit -> bool = "ml_zompRequestedPause"
external zompSetRequestPause : request:bool -> unit
  = "ml_zompSetRequestPause"
external zompDoesVerifyCode : unit -> bool = "ml_zompDoesVerifyCode"
external zompVerifyCode : doit:bool -> unit = "ml_zompVerifyCode"
external zompOptimizeFunction : unit -> bool = "ml_zompOptimizeFunction"
external zompSetOptimizeFunction : optimize:bool -> unit
  = "ml_zompSetOptimizeFunction"
external zompConnectToRemoteVM : uri:string -> bool
  = "ml_zompConnectToRemoteVM"
external zompDisconnectRemoteVM : unit -> unit = "ml_zompDisconnectRemoteVM"
external zompSendToRemoteVM : uri:string -> unit = "ml_zompSendToRemoteVM"
external zompSendCode : code:string -> moduleName:string -> bool
  = "ml_zompSendCode"
external zompOptimizeFunctions : unit -> unit = "ml_zompOptimizeFunctions"
external zompLoadFile : filename:string -> bool = "ml_zompLoadFile"
external zompPrintModuleCode : unit -> unit = "ml_zompPrintModuleCode"
external zompWriteLLVMCodeToFile : fileName:string -> unit
  = "ml_zompWriteLLVMCodeToFile"
external zompRemoveFunctionBody : funcName:string -> bool
  = "ml_zompRemoveFunctionBody"
external zompRecompileAndRelinkFunction : funcName:string -> bool
  = "ml_zompRecompileAndRelinkFunction"
external zompResetArgs : unit -> unit = "ml_zompResetArgs"
external zompAddIntArg : arg:int -> unit = "ml_zompAddIntArg"
external zompAddPointerArg : ptr:cptr -> unit = "ml_zompAddPointerArg"
external zompRunFunction : name:string -> unit = "ml_zompRunFunction"
external zompRunFunctionInt : name:string -> int = "ml_zompRunFunctionInt"
external zompRunFunctionIntWithArgs : name:string -> int
  = "ml_zompRunFunctionIntWithArgs"
external zompRunFunctionPointerWithArgs : name:string -> cptr
  = "ml_zompRunFunctionPointerWithArgs"
external zompRunFunctionString : name:string -> string
  = "ml_zompRunFunctionString"
external zompRunFunctionStringWithArgs : name:string -> string
  = "ml_zompRunFunctionStringWithArgs"
external zompRunFunctionBool : name:string -> bool = "ml_zompRunFunctionBool"
external zompLoadLib : name:string -> int = "ml_zompLoadLib"
external zompSimpleAst : name:string -> cptr = "ml_zompSimpleAst"
external zompAddChild : parent:cptr -> child:cptr -> unit = "ml_zompAddChild"
external zompAstId : ast:cptr -> string = "ml_zompAstId"
external zompAstChildCount : ast:cptr -> int = "ml_zompAstChildCount"
external zompAstChild : ast:cptr -> num:int -> cptr = "ml_zompAstChild"
external zompSetAstLocation :
  ast:cptr -> file:string -> line:int -> column:int -> unit
  = "ml_zompSetAstLocation"
external zompAstFile : ast:cptr -> string = "ml_zompAstFile"
external zompAstLine : ast:cptr -> int = "ml_zompAstLine"
external zompAstColumn : ast:cptr -> int = "ml_zompAstColumn"
external zompResetMacroArgs : unit -> unit = "ml_zompResetMacroArgs"
external zompAddMacroArg : ptr:cptr -> unit = "ml_zompAddMacroArg"
external zompAddressOfMacroFunction : name:string -> cptr
  = "ml_zompAddressOfMacroFunction"
external zompCallMacro : macroAddress:cptr -> cptr = "ml_zompCallMacro"
external float2string : f:double -> string = "ml_float2string"
external zompPrintTimingStats : unit -> unit = "ml_zompPrintTimingStats"
external zompAstIsNull : ptr:cptr -> bool = "ml_zompAstIsNull"
external zompIsDebugBuild : unit -> bool = "ml_zompIsDebugBuild"
external zompBuildInfo : unit -> string = "ml_zompBuildInfo"
module StatisticsBackend : sig  end
val zompvmSection : Common.Statistics.section
module NativeAst :
  sig
    val isValidId : string -> bool
    val extractSExprFromNativeAst : cptr -> Ast2.sexpr
    val applyLocation : cptr -> Ast2.sexpr -> cptr
    val buildNativeAst : Ast2.sexpr -> cptr
  end
val raiseFailedToEvaluateLLVMCode : string -> string -> 'a
type targetModule = Runtime | Compiletime
val evalLLVMCodeB :
  ?targetModule:targetModule -> string list -> 'a -> string -> unit
val evalLLVMCode :
  ?targetModule:targetModule ->
  Bindings.bindings -> [> `DefineFunc of Lang.func ] list -> string -> unit
val loadLLVMFile : string -> unit
val currentBindings : Bindings.t ref
val isInteractiveFlag : bool ref
val isInteractive : unit -> bool
val setIsInteractive : bool -> unit
val traceMacroExpansionOn : bool ref
val flushStreams : unit -> unit
module Callbacks : sig  end
