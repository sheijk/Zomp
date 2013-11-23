
(** Compilation environment carrying the compiler state *)
type env
val createEnv : Bindings.t -> env
val bindings : env -> Bindings.t

val loadPrelude :
  env ->
  ?emitBackendCode:(string -> unit) ->
  ?appendSource:string ->
  string ->
  (* Bindings.t *)
  unit Result.t

val compileExpr : env -> Ast2.t -> Lang.toplevelExpr Result.t * string

val compileFromStream :
  env ->
  source:string ->
  emitBackendCode:(string -> unit) ->
  fileName:string ->
  Lang.toplevelExpr Result.t


val writeSymbolsToStream : Bindings.bindings -> out_channel -> unit
val writeSymbols : Bindings.bindings -> string -> bool
