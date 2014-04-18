(** Utility functions to compile code. **)

val loadPrelude :
  Expander.tlenv ->
  ?appendSource:string ->
  (* source *) string ->
  unit Result.t

val compileExpr : Expander.tlenv -> Ast2.t -> Lang.toplevelExpr Result.t * string

val compileFromStream :
  Expander.tlenv ->
  source:string ->
  fileName:string ->
  Lang.toplevelExpr Result.t

val writeSymbolsToStream : Bindings.bindings -> out_channel -> unit
val writeSymbols : Bindings.bindings -> string -> bool

