exception CatchedError of Serror.t list
val signalErrors : Serror.t list -> 'a
exception CouldNotParse of Serror.t
val translateTLNoError :
  Bindings.t -> Ast2.sexpr -> Bindings.t * Lang.toplevelExpr list
val compileExpr :
  ('a ->
   'b ->
   'c *
   ([< `DefineFunc of Lang.func
     | `GlobalVar of Lang.globalVar
     | `Typedef of string * Lang.typ ]
    as 'd)
   list) ->
  'a -> 'b -> 'c * 'd list * string
val parse :
  ('a -> Ast2.sexpr) ->
  'a ->
  Bindings.t -> Lang.toplevelExpr list -> Bindings.t * Lang.toplevelExpr list
val catchingErrorsDo : (unit -> 'a) -> onErrors:(Serror.t list -> 'a) -> 'a
val compile :
  readExpr:(Bindings.t -> Ast2.sexpr option) ->
  ?beforeCompilingExpr:(Ast2.sexpr -> unit) ->
  onSuccess:(Ast2.sexpr ->
             Bindings.t ->
             Bindings.t -> Lang.toplevelExpr list -> string -> unit) ->
  onErrors:(Serror.t list -> unit) -> Bindings.t -> Bindings.t
val compileCode :
  Bindings.t -> string -> out_channel -> string -> Bindings.t option
val loadPrelude :
  ?processExpr:(Ast2.sexpr ->
                Bindings.t ->
                Bindings.t -> Lang.toplevelExpr list -> string -> unit) ->
  ?appendSource:string -> string -> Bindings.t
val writeSymbolsToStream : Bindings.bindings -> out_channel -> unit
val writeSymbols : Bindings.bindings -> string -> bool
