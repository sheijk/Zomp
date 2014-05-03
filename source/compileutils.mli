(** Utility functions to compile code. **)

val loadPrelude :
  Expander.tlenv ->
  ?appendSource:string ->
  (* source *) string ->
  unit Result.t

val compileFromStream :
  Expander.tlenv ->
  source:string ->
  fileName:string ->
  Lang.toplevelExpr Result.t

val writeSymbolsToStream : Bindings.bindings -> out_channel -> unit
val writeSymbols : Bindings.bindings -> string -> bool

