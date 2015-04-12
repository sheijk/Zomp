(** Backend generating a textual representation of LLVM byte code. **)

exception CodeGenError of string
val raiseCodeGenError : msg:string -> 'a

(** The backend. *)
type t
val create : unit -> t

(** The size_t type for this backend *)
val sizeTType : t -> Types.typ

(** The default symbols. *)
val defaultBindings : Bindings.bindings

(** Generate code for the given expressions. The code will directly be sent for
    evaluation to the VM. *)
val gencodeTL : t -> Zompvm.phase -> Lang.toplevelExpr -> unit

