(** Interface to the virtual machine (LLVM). **)

type cptr
val isNullPtr : cptr -> bool

(** Create and initialize the VM *)
val init : unit -> bool
(** Shutdown the VM. De-allocates all resources. Using any function in this
module after calling this is illegal. *)
val shutdown : unit -> unit

(** Will verify generated LLVM code if this is true. *)
val setVerifyCode : bool -> unit
val verifyCode : unit -> bool
(** Will run a set of standard optimizations on every function before generating
native code. *)
val autoOptimizeFunctions : unit -> bool
val setAutoOptimizeFunctions : bool -> unit

(** Will run a set of standard optimizations on all functions and re-generate
native code. *)
val optimizeCode : unit -> unit

(** Humand readable description of build variant, target, etc. *)
val buildInfo : unit -> string
(** Whether this build has debug info. *)
val isDebugBuild : unit -> bool
(** Will print some timing statistics to stdout. *)
val printTimingStats : unit -> unit
(** Will write all generated LLVM code to the given file (for debugging). *)
val writeLLVMCodeToFile : string -> unit
(** Will dump the LLVM module. *)
val printModuleCode : unit -> unit

(** Convert between OCaml and native representation of ASTs. *)
module NativeAst : sig
  type t

  val addr : t -> cptr

  val isValidId : string -> bool
  val extractSExprFromNativeAst : t -> Ast2.sexpr
  val applyLocation : t -> Ast2.sexpr -> t
  val buildNativeAst : Ast2.sexpr -> t

  val simple : string -> t
  val isNull : t -> bool
  val addChild : t -> t -> unit
end

(** Call LLVM functions. *)
module Call : sig
  val reset : unit -> unit
  val addPointerArg : cptr -> unit
  val addIntArg : int -> unit

  val void : name:string -> unit
  val string : name:string -> string
  val int : name:string -> int
  val pointer : name:string -> cptr
  val bool : name:string -> bool
end

(** Call LLVM macro functions. *)
module Macros : sig
  type func

  val resetArgs : unit -> unit
  val addArg : NativeAst.t -> unit
  val call : func -> NativeAst.t

  val addressOfMacroFunction : name:string -> func
end

(** Interact with remote VM. *)
module Remote : sig
  val connect : uri:string -> bool
  val disconnect : unit -> unit

  val send : uri:string -> unit
end

val removeFunctionBodies : string list -> unit
val relinkFunctions : string list -> unit

type code
val codeFromLlvm : string -> code
val codeToString : code -> string

type phase = CompilationPhase | CompilationAndRuntimePhase
val evalCode : phase -> code -> unit
val registerCodeHandler : (code -> unit) -> unit
val unregisterCodeHandler : (code -> unit) -> unit

(** Load file containing textual representation of LLVM byte code and evaluate
    it. Does not throw but prints errors to stderr. *)
val loadLLVMFile : string -> unit


(** Handle for a loaded DLL *)
module DllHandle : sig
  type t
  val addr : t -> cptr
end

(** Loads a dynamic library. *)
val loadLib : string -> DllHandle.t

(** Hack to work around lack of passing environment to macros. *)
val currentBindings : Bindings.t ref

(** Will be false if running compiler and true if running Zomp shell. *)
val isInteractive : unit -> bool
val setIsInteractive : bool -> unit

val traceMacroExpansionOn : bool ref

(** Will flush all stdout streams (OCaml, C stdout, iostreams, LLVM streams). *)
val flushStreams : unit -> unit


