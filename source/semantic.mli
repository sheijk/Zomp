type formOrAst = Form of Lang.form | Ast of Ast2.t
type typeRequirement =
    [ `Any of string
    | `Array of Lang.typ * int
    | `Bool
    | `Char
    | `Double
    | `ErrorType of string
    | `Float
    | `Function of Lang.functionType
    | `Int16
    | `Int32
    | `Int64
    | `Int8
    | `ParametricType of Lang.typ Lang.parameterizableType
    | `Pointer of Lang.typ
    | `Record of Lang.typ Lang.recordType
    | `TypeParam
    | `TypeRef of string
    | `Void ]
val typeRequirementToString :
  [< `Any of string
   | `Array of Lang.typ * int
   | `Bool
   | `Char
   | `Double
   | `ErrorType of string
   | `Float
   | `Function of Lang.functionType
   | `Int16
   | `Int32
   | `Int64
   | `Int8
   | `ParametricType of Lang.typ Lang.parameterizableType
   | `Pointer of Lang.typ
   | `Record of Lang.typ Lang.recordType
   | `TypeParam
   | `TypeRef of string
   | `Void ] ->
  string
type typecheckResult =
    TypeOf of Lang.composedType
  | TypeError of formOrAst * string * Lang.composedType * typeRequirement
val equalTypes :
  Bindings.bindings -> Lang.composedType -> Lang.composedType -> bool
val typeCheckFuncCall :
  (Bindings.bindings -> Lang.form -> typecheckResult) ->
  Bindings.bindings ->
  Lang.form -> Lang.form Lang.funcCall -> typecheckResult
val typeCheck : Bindings.bindings -> Lang.form -> typecheckResult
val collectVars :
  Lang.form -> Lang.form * Lang.composedType Lang.variable list
val moveLocalVarsToEntryBlock : Lang.form -> Lang.form list
val test_moveLocalVarsToEntryBlock : unit -> unit
val lookupTypeInBindings :
  Bindings.bindings -> string -> [> `Found of Lang.composedType | `NotFound ]
val typesEquivalent :
  Bindings.bindings -> Lang.composedType -> Lang.composedType -> bool
val typeCheckTL :
  Bindings.bindings ->
  [< `DefineFunc of Lang.func | `GlobalVar of Lang.composedType Lang.variable ] ->
  typecheckResult
val typeOfForm :
  onError:(msg:string ->
           found:Lang.composedType ->
           expected:typeRequirement -> Lang.composedType) ->
  Bindings.bindings -> Lang.form -> Lang.composedType
val functionIsValid : Lang.func -> [ `Errors of string list | `Ok ]
