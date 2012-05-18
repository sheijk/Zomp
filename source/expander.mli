
(**
   Signalled by old code which cannot report errors properly. Will be replaced
   by returning Errors.t
*)
exception IllegalExpression of Ast2.sexpr * string

type 'a mayfail = private Result of 'a | Error of string list
val errorFromString : string -> 'a mayfail
val errorFromExpr : Ast2.sexpr -> string -> 'a mayfail
val result : 'a -> 'a mayfail

type toplevelEnv
val envBindings : toplevelEnv -> Bindings.t

type toplevelTranslationResult =
    (Bindings.t * Lang.toplevelExpr list) mayfail

val tlReturnNoExprs : toplevelEnv -> toplevelTranslationResult

val translateTL :
  Bindings.t ->
  Ast2.sexpr ->
  (Bindings.t * Lang.toplevelExpr list) mayfail


(**
   Required to set include paths and add LLVM code evaluation. Should be
   cleaned up
*)

type toplevelTranslationFunction = toplevelEnv -> Ast2.sexpr -> toplevelTranslationResult

val addToplevelInstruction :
  string -> toplevelTranslationFunction -> unit

val makeTranslateSeqFunction :
  (string -> unit) -> toplevelTranslationFunction
val makeTranslateIncludeFunction :
  string list ref -> (string -> unit) -> toplevelTranslationFunction

val setTraceMacroExpansion : (string -> Ast2.sexpr -> unit) option -> unit

