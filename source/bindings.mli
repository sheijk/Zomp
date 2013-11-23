
type symbol =
    VarSymbol of Lang.composedType Lang.variable
  | FuncSymbol of Lang.func
  | MacroSymbol of bindings Lang.macro
  | TypedefSymbol of Lang.composedType
  | LabelSymbol of Lang.label
  | UndefinedSymbol
and symbolInfo
and bindings
type t = bindings

val defaultBindings : bindings
val fromSymbolList : (string * symbol) list -> bindings
val addVar :
  bindings -> Lang.composedType Lang.variable -> bindings
val addFunc : bindings -> Lang.func -> bindings
val addTypedef :
  bindings -> string -> Lang.composedType -> Basics.location -> bindings
val addLabel : bindings -> string -> bindings
val addMacro :
  bindings -> string -> string -> Basics.location ->
  (bindings -> Ast2.sexpr -> Ast2.sexpr) -> bindings
val lookup : bindings -> string -> symbol
val lookupInfo : bindings -> string -> symbolInfo
val location : symbolInfo -> Basics.location option
val symbol : symbolInfo -> symbol
val isFunction : bindings -> string -> bool
val kindToString : symbol -> string

val iter : ((string * symbol) -> unit) -> bindings -> unit
val iterInfo : ((string * symbolInfo) -> unit) -> bindings -> unit

