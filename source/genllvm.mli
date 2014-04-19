(** Backend generating a textual representation of LLVM byte code. **)

exception CodeGenError of string
val raiseCodeGenError : msg:string -> 'a

(** The default symbols. *)
val defaultBindings : Bindings.bindings

(** Generate code for the given expressions. *)
val gencodeTL : Lang.toplevelExpr -> string

