val combine : string -> string list -> string
exception CodeGenError of string
val raiseCodeGenError : msg:string -> 'a
val typeOfForm : Bindings.bindings -> Lang.form -> Lang.composedType
val escapeName : string -> string
val llvmName : string -> string
val llvmTypeName : Lang.typ -> string
val llvmTypeNameLong : Lang.typ -> string
val paramTypeName : Lang.typ -> string
type resultvar = { rvname : string; rvtypename : string; }
val resultVar : Lang.typ Lang.variable -> resultvar
val noVar : resultvar
type gencodeResult = {
  gcrVar : resultvar;
  gcrCode : string;
  gcrFirstBBCode : string option;
}
val returnVarCode : resultvar * string -> gencodeResult
val return : resultvar * string * string -> gencodeResult
val returnCombi : resultvar * string * string option list -> gencodeResult
val lastTempVarNum : int ref
val nextUID : unit -> int
val newGlobalTempVar : ?base:string -> Lang.typ -> resultvar
val newLocalTempVar : ?base:string -> Lang.typ -> resultvar
val isConstant : string -> bool
val paramString : (string * Lang.typ) list -> string
val stringMap : (char -> string) -> string -> string
val llvmEscapedString : string -> string
val isValidLlvmString : string -> bool
val lastUniqueId : int ref
val newUniqueId : unit -> int
val newUniqueName : unit -> string
val sexpr2codeNoAntiquotes :
  (Ast2.sexpr -> Ast2.sexpr) -> Ast2.sexpr -> Ast2.sexpr
val sexpr2codeasis : Ast2.sexpr -> Ast2.sexpr
val sexpr2code :
  ?antiquoteF:(string -> Ast2.sexpr list -> Ast2.sexpr) ->
  Ast2.sexpr -> Ast2.sexpr
val insertAstConstructors :
  Bindings.bindings -> string -> Ast2.sexpr list -> Ast2.sexpr
val defaultBindings : Bindings.bindings
val externalFuncDecls : string
val findIntrinsic : string -> (string list -> string) option
val gencodeSequence : ('a -> gencodeResult) -> 'a list -> gencodeResult
val gencodeDefineVariable :
  ('a -> gencodeResult) ->
  Lang.typ Lang.variable -> 'a option -> gencodeResult
val gencodeVariable : Lang.typ Lang.variable -> gencodeResult
val llvmValue : Lang.value -> string
val gencodeConstant : Lang.value -> gencodeResult
val gencodeFuncCall :
  (Lang.form -> gencodeResult) -> Lang.form Lang.funcCall -> gencodeResult
val offsetStringAndCode :
  ([> `Variable of Lang.composedType Lang.variable ] -> resultvar * string) ->
  [> `Constant of Lang.value
   | `Variable of
       [< `Array of Lang.typ * int
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
        | `ParametricType of [< Lang.typ Lang.parameterizableType ]
        | `Pointer of Lang.typ
        | `Record of Lang.typ Lang.recordType
        | `TypeParam
        | `TypeRef of string
        | `Void ]
       Lang.variable ] ->
  string * string
val checkType : resultvar -> Lang.typ -> unit
val todoBindings : Bindings.bindings
val gencodeGenericIntr :
  (Lang.form -> gencodeResult) ->
  [< `CastIntrinsic of Lang.typ * Lang.form
   | `GetAddrIntrinsic of Lang.typ Lang.variable
   | `GetFieldPointerIntrinsic of Lang.form * string
   | `LoadIntrinsic of Lang.form
   | `MallocIntrinsic of Lang.typ * Lang.form
   | `PtrAddIntrinsic of Lang.form * Lang.form
   | `PtrDiffIntrinsic of Lang.form * Lang.form
   | `StoreIntrinsic of Lang.form * Lang.form ] ->
  gencodeResult
val gencodeAssignVar :
  ('a -> gencodeResult) -> Lang.typ Lang.variable -> 'a -> gencodeResult
val gencodeReturn :
  (Lang.form -> gencodeResult) -> Lang.form -> gencodeResult
val gencodeJump : Lang.label -> gencodeResult
val gencodeLabel : Lang.label -> gencodeResult
val gencodeBranch :
  ([> `Variable of Lang.typ Lang.variable ] -> gencodeResult) ->
  Lang.branch -> gencodeResult
val gencodeEmbeddedComment : string list -> gencodeResult
val gencode : Lang.form -> gencodeResult
val countChar : string -> char -> int
val llvmStringLength : string -> int
val gencodeGlobalVar : Lang.globalVar -> string
val gencodeDefineFunc : Lang.func -> string
val gencodeTypedef : string -> Lang.typ -> string
val gencodeTL :
  [< `DefineFunc of Lang.func
   | `GlobalVar of Lang.globalVar
   | `Typedef of string * Lang.typ ] ->
  string
val genmodule : Lang.toplevelExpr list -> string
