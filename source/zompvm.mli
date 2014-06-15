(** Interface to the virtual machine (LLVM). **)

type cptr = Machine.cptr
val isNullPtr : cptr -> bool

val init : unit -> bool
val shutdown : unit -> unit

val setVerifyCode : bool -> unit
val verifyCode : unit -> bool
val autoOptimizeFunctions : unit -> bool
val setAutoOptimizeFunctions : bool -> unit

val optimizeCode : unit -> unit

val buildInfo : unit -> string
val isDebugBuild : unit -> bool
val printTimingStats : unit -> unit
val writeLLVMCodeToFile : string -> unit
val printModuleCode : unit -> unit

(** Convert between OCaml and native representation of ASTs. *)
module NativeAst : sig
  val isValidId : string -> bool
  val extractSExprFromNativeAst : cptr -> Ast2.sexpr
  val applyLocation : cptr -> Ast2.sexpr -> cptr
  val buildNativeAst : Ast2.sexpr -> cptr

  val simple : string -> cptr
  val isNull : cptr -> bool
  val addChild : cptr -> cptr -> unit
end

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

module Macros : sig
  val resetArgs : unit -> unit
  val addArg : cptr -> unit
  val call : cptr -> cptr

  val addressOfMacroFunction : name:string -> cptr
end

module Remote : sig
  val connect : uri:string -> bool
  val disconnect : unit -> unit

  val send : uri:string -> unit
end

(** Remove bodies of functions, evaluate given LLVM code and then recompile and
    relink the functions. First parameter are the names of the functions, second 
    is the LLVM code. *)
val evalLLVMCodeB : string list -> string -> unit
(** Will identify all function definitions in the toplevel expressions and call
    evalLLVMCodeB with it's names. *)
val evalLLVMCode : Lang.toplevelExpr list -> string -> unit

(** Load file containing textual representation of LLVM byte code and evaluate
    it. Does not throw but prints errors to stderr. *)
val loadLLVMFile : string -> unit

(** Loads a dynamic library. *)
val loadLib : string -> cptr

(** Hack to work around lack of passing environment to macros. *)
val currentBindings : Bindings.t ref

(** Will be false if running compiler and true if running Zomp shell. *)
val isInteractive : unit -> bool
val setIsInteractive : bool -> unit

val traceMacroExpansionOn : bool ref

(** Will flush all stdout streams (OCaml, C stdout, iostreams, LLVM streams). *)
val flushStreams : unit -> unit


