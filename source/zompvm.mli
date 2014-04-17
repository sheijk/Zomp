type cptr = Machine.cptr
val isNullPtr : cptr -> bool

(** Convert between OCaml and native representation of ASTs. *)
module NativeAst : sig
  val isValidId : string -> bool
  val extractSExprFromNativeAst : cptr -> Ast2.sexpr
  val applyLocation : cptr -> Ast2.sexpr -> cptr
  val buildNativeAst : Ast2.sexpr -> cptr
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

(** Hack to work around lack of passing environment to macros. *)
val currentBindings : Bindings.t ref

(** Will be false if running compiler and true if running Zomp shell. *)
val isInteractive : unit -> bool
val setIsInteractive : bool -> unit

val traceMacroExpansionOn : bool ref

(** Will flush all stdout streams (OCaml, C stdout, iostreams, LLVM streams). *)
val flushStreams : unit -> unit


