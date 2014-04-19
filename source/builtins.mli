(** Definition of built-in macros, functions, etc. *)

type backendInfo = {
  sizeT :Typesystems.Zomp.typ;
}

(** The default symbols. *)
val defaultBindings : backendInfo -> Bindings.bindings

(** Built-in functions that every back-end has to support. *)
val builtinIntrinsics : backendInfo -> Lang.func list

