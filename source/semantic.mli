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

val typeCheckTL :
  Bindings.bindings ->
  [< `DefineFunc of Lang.func | `GlobalVar of Lang.typ Lang.variable ] ->
  typecheckResult

(** Can throw Failure if an invalid AST is found. First argument is sizeT *)
val typeOfForm : Types.typ -> Lang.form -> Lang.typ

(** Produces a list of basic blocks with the following guarantees:
- all instructions will be in three address form
  (all arguments will either be `Constant or `Variable)
- all blocks end with an instruction transfering control flow
- the first block is the one to be run first

First argument is the backend's sizeT
 *)
val splitBasicBlocks : Types.typ -> Basics.location -> Lang.typ -> Lang.form -> int * (string * Lang.form list) list

val functionIsValid : Lang.func -> [ `Errors of string list | `Ok ]

val sideEffectFree : Lang.form -> bool

