(** Expand macros and recognize std:base language. **)

(**
   Signalled by old code which cannot report errors properly. Will be replaced
   by returning Errors.t
*)
exception IllegalExpression of Ast2.sexpr * Serror.t list

type tlenv
val createEnv : Bindings.t -> onError:(Serror.t -> unit) -> tlenv
(** Errors should all get reported by calling the function passed to createEnv.
This is not working in all cases, yet so be sure to handle both cases *)
val translate : tlenv -> Ast2.t -> Lang.toplevelExpr Result.t
(** Errors should all get reported by calling the function passed to createEnv.
This is not working in all cases, yet so be sure to handle both cases *)
val translateMulti : tlenv -> Ast2.t list -> Lang.toplevelExpr Result.t
val bindings : tlenv -> Bindings.t
val setBindings : tlenv -> Bindings.t -> unit
val emitExpr : tlenv -> Ast2.t -> unit
val emitError : tlenv -> Serror.t -> unit

val addDllPath : tlenv -> string -> [`Front | `Back] -> unit
val recommendedDllPath : string list
val addIncludePath : tlenv -> string -> [`Front | `Back] -> unit
val recommendedIncludePath : string list

val addTranslateFunction : string -> doc:string -> (tlenv -> Ast2.sexpr -> unit) -> unit

val setTraceToplevelForm : (Lang.toplevelExpr -> unit) option -> unit
val setTraceMacroExpansion : (string -> Ast2.sexpr -> unit) option -> unit

val foreachToplevelBaseInstructionDoc : (string -> string -> unit) -> unit
val foreachBaseInstructionDoc : (string -> string -> unit) -> unit

