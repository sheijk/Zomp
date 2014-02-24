
(**
   Signalled by old code which cannot report errors properly. Will be replaced
   by returning Errors.t
*)
exception IllegalExpression of Ast2.sexpr * Serror.t list

type tlenv
val createEnv : Bindings.t -> tlenv
val compileExpr : tlenv -> Ast2.t -> Lang.toplevelExpr Result.t * string
val translate : tlenv -> Ast2.t -> Lang.toplevelExpr Result.t
val translateMulti : (string -> unit) -> tlenv -> Ast2.t list -> Lang.toplevelExpr Result.t
val bindings : tlenv -> Bindings.t
val setBindings : tlenv -> Bindings.t -> unit
val emitExpr : tlenv -> Ast2.t -> unit
val emitError : tlenv -> Serror.t -> unit

val addDllPath : tlenv -> string -> [`Front | `Back] -> unit
val addIncludePath : tlenv -> string -> [`Front | `Back] -> unit

val addTranslateFunction : string -> doc:string -> (tlenv -> Ast2.sexpr -> unit) -> unit

val setEmitbackendCode : (string -> unit) -> unit

val setTraceMacroExpansion : (string -> Ast2.sexpr -> unit) option -> unit

val foreachToplevelBaseInstructionDoc : (string -> string -> unit) -> unit
val foreachBaseInstructionDoc : (string -> string -> unit) -> unit

