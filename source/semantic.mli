(** Type checking and a bit of semantic analysis. **)

type formOrAst = Form of Lang.form | Ast of Ast2.t

type typeRequirement = 
  [ `Any of string
  | Types.typ ]
val typeRequirementToString : typeRequirement -> string

type typecheckResult =
    TypeOf of Lang.typ
  | TypeError of formOrAst * string * Lang.typ * typeRequirement

val equalTypes :
  Bindings.bindings -> Lang.typ -> Lang.typ -> bool

val typeCheck : Bindings.bindings -> Lang.form -> typecheckResult

val moveLocalVarsToEntryBlock : Lang.form -> Lang.form list

val collectFunctionDefinitions : Lang.toplevelExpr list -> string list

val typeCheckTL :
  Bindings.bindings ->
  [< `DefineFunc of Lang.func | `GlobalVar of Lang.typ Lang.variable ] ->
  typecheckResult

val typeOfForm :
  onError:(msg:string ->
           found:Lang.typ ->
           expected:typeRequirement -> Lang.typ) ->
  Bindings.bindings -> Lang.form -> Lang.typ

val functionIsValid : Lang.func -> [ `Errors of string list | `Ok ]

