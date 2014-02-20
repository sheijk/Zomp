
val loadPrelude :
  Expander.tlenv ->
  ?emitBackendCode:(string -> unit) ->
  ?appendSource:string ->
  string ->
  (* Bindings.t *)
  unit Result.t

val compileExpr : Expander.tlenv -> Ast2.t -> Lang.toplevelExpr Result.t * string

val compileFromStream :
  Expander.tlenv ->
  source:string ->
  emitBackendCode:(string -> unit) ->
  fileName:string ->
  Lang.toplevelExpr Result.t


val writeSymbolsToStream : Bindings.bindings -> out_channel -> unit
val writeSymbols : Bindings.bindings -> string -> bool
