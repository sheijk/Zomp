
(** Throws an exception which will be caught by catchingErrorsDo. Used for
legacy code *)
val signalErrors : Serror.t list -> 'a

val catchingErrorsDo : (unit -> 'a) -> onErrors:(Serror.t list -> 'a) -> 'a

val loadPrelude :
  ?processExpr:(Ast2.sexpr ->
                Bindings.t ->
                Bindings.t -> Lang.toplevelExpr list -> string -> unit) ->
  ?appendSource:string -> string -> Bindings.t

(** Compilation environment carrying the compiler state *)
type env
val createEnv : Bindings.t -> env
val compileExprNew : env -> Ast2.t ->
  Result.flag * Serror.t list * Lang.toplevelExpr list * string
val compileNew : env -> string -> out_channel -> string -> Lang.toplevelExpr Result.t
val bindings : env -> Bindings.t

val writeSymbolsToStream : Bindings.bindings -> out_channel -> unit
val writeSymbols : Bindings.bindings -> string -> bool
