
(**
 * Signalled by old code which cannot report errors properly. Will be replaced
 * by returning Errors.t
 *)
exception IllegalExpression of Ast2.sexpr * string

type 'a mayfail = private Result of 'a | Error of string list
val errorFromString : string -> 'a mayfail

type toplevelTranslationResult =
    (Bindings.t * Lang.toplevelExpr list) mayfail

type toplevelEnv
val tlReturnNoExprs : toplevelEnv -> toplevelTranslationResult

val translateSeqTL :
  (string -> unit) ->
  toplevelEnv ->
  Ast2.sexpr ->
  toplevelTranslationResult

val toplevelBaseInstructions :
  (string,
   toplevelEnv ->
   Ast2.sexpr -> toplevelTranslationResult)
  Hashtbl.t

val translateTL :
  Bindings.t -> Ast2.sexpr -> Bindings.t * Lang.toplevelExpr list

val translateInclude :
  string list ref ->
  (string -> unit) ->
  toplevelEnv ->
  Ast2.sexpr ->
  toplevelTranslationResult

