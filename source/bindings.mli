
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
  bindings -> string -> Lang.composedType -> bindings
val addLabel : bindings -> string -> bindings
val addMacro :
  bindings -> string -> string ->
  (bindings -> Ast2.sexpr list -> Ast2.sexpr) -> bindings
val lookup : bindings -> string -> symbol
val lookupInfo : bindings -> string -> symbolInfo
val isFunction : bindings -> string -> bool

val iter : ((string * symbol) -> unit) -> bindings -> unit

