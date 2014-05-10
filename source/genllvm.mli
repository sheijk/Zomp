(** Backend generating a textual representation of LLVM byte code. **)

exception CodeGenError of string
val raiseCodeGenError : msg:string -> 'a

(** The backend. *)
type t
val create : unit -> t

(** The default symbols. *)
val defaultBindings : Bindings.bindings

(** Generate code for the given expressions. *)
val gencodeTL : t -> Lang.toplevelExpr -> string

