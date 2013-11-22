
(** Throws an exception which will be caught by catchingErrorsDo. Used for
legacy code *)
val signalErrors : Serror.t list -> 'a

val catchingErrorsDo : (unit -> 'a) -> onErrors:(Serror.t list -> 'a) -> 'a

val loadPrelude :
  ?emitBackendCode:(string -> unit) ->
  ?appendSource:string -> string -> Bindings.t

(** Compilation environment carrying the compiler state *)
type env
val createEnv : Bindings.t -> env
val bindings : env -> Bindings.t

val compileExprNew : env -> Ast2.t ->
  Result.flag * Serror.t list * Lang.toplevelExpr list * string

val compileFromStream :
  env ->
  source:string ->
  emitBackendCode:(string -> unit) ->
  fileName:string ->
  Lang.toplevelExpr Result.t


val writeSymbolsToStream : Bindings.bindings -> out_channel -> unit
val writeSymbols : Bindings.bindings -> string -> bool
