type cptr = Machine.cptr
val evalLLVMCodeB : string list -> string -> unit
val evalLLVMCode : Lang.toplevelExpr list -> string -> unit
val loadLLVMFile : string -> unit
val currentBindings : Bindings.t ref
val isInteractiveFlag : bool ref
val isInteractive : unit -> bool
val setIsInteractive : bool -> unit
val traceMacroExpansionOn : bool ref
val flushStreams : unit -> unit
module Callbacks : sig  end

val isNullPtr : cptr -> bool

