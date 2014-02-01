
(**
   Signalled by old code which cannot report errors properly. Will be replaced
   by returning Errors.t
*)
exception IllegalExpression of Ast2.sexpr * Serror.t list

module Mayfail : sig
  type 'a mayfail = private Result of 'a | Error of Serror.t list

  val errorFromString : Basics.location -> string -> 'a mayfail
  val errorFromExpr : Ast2.sexpr -> string -> 'a mayfail
  val singleError : Serror.t -> 'a mayfail
  val multipleErrors : Serror.t list -> 'a mayfail
  val result : 'a -> 'a mayfail
end

type toplevelEnv
val envBindings : toplevelEnv -> Bindings.t

type tlenv
val createEnv : Bindings.t -> tlenv
val translate : tlenv -> Ast2.t -> Lang.toplevelExpr Result.t
val bindings : tlenv -> Bindings.t

type toplevelTranslationResult = (Bindings.t * Lang.toplevelExpr list) Mayfail.mayfail

(**
   Required to set include paths and add LLVM code evaluation. Should be
   cleaned up
*)
type toplevelTranslationFunction

val makeToplevelInstruction : (toplevelEnv -> Ast2.sexpr -> toplevelTranslationResult) -> toplevelTranslationFunction

(** name, parameter doc, translation func *)
val addToplevelInstruction :
  string -> string -> toplevelTranslationFunction -> unit

val makeTranslateSeqFunction :
  (string -> unit) -> toplevelTranslationFunction
val makeTranslateIncludeFunction :
  string list ref -> (string -> unit) -> toplevelTranslationFunction
val translateLinkCLib : string list ref -> toplevelTranslationFunction

val setTraceMacroExpansion : (string -> Ast2.sexpr -> unit) option -> unit

val foreachToplevelBaseInstructionDoc : (string -> string -> unit) -> unit
val foreachBaseInstructionDoc : (string -> string -> unit) -> unit

