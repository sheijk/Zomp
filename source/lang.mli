val macroVar : string
val macroVar2 : string
val macroFunc : string
val macroAssign : string
val macroSequence : string
val macroTypedef : string
val macroField : string
val macroPtr : string
val macroReturn : string
val macroLabel : string
val macroBranch : string
val macroMacro : string
val macroReplacement : string
val macroFieldptr : string
val macroLoad : string
val macroStore : string
val macroNullptr : string
val macroPtradd : string
val macroPtrDiff : string
val macroMalloc : string
val macroGetaddr : string
val macroCast : string
val macroInclude : string
val macroRest : string
val macroJuxOp : string
val macroSeqOp : string
val macroCallOp : string
val macroApply : string
val macroFunCall : string
val macroParamType : string
val macroError : string
val macroLinkCLib : string

val componentType : ('a * 'b) list -> 'a -> 'b option
val componentNum : ('a * 'b) list -> 'a -> int
type intType = [ `Int16 | `Int32 | `Int64 | `Int8 ]
type integralType =
    [ `Bool
    | `Char
    | `Double
    | `Float
    | `Int16
    | `Int32
    | `Int64
    | `Int8
    | `Void ]
type 'a parameterizableType = [ `Pointer of 'a | `Record of 'a recordType ]
and 'a recordType =
  'a Typesystems.Zomp.recordType = {
  rname : string;
  fields : (string * 'a) list;
}
type typ =
    [ `Array of typ * int
    | `Bool
    | `Char
    | `Double
    | `ErrorType of string
    | `Float
    | `Function of functionType
    | `Int16
    | `Int32
    | `Int64
    | `Int8
    | `ParametricType of typ parameterizableType
    | `Pointer of typ
    | `Record of typ recordType
    | `TypeParam
    | `TypeRef of string
    | `Void ]
and functionType =
  Typesystems.Zomp.functionType = {
  returnType : typ;
  argTypes : typ list;
}
val bitcount : intType -> int
val isTypeParametric : typ -> bool
type value =
  Typesystems.Zomp.value =
    VoidVal
  | Int8Val of Int32.t
  | Int16Val of Int32.t
  | Int32Val of Int32.t
  | Int64Val of Int64.t
  | FloatVal of float
  | DoubleVal of float
  | StringLiteral of string
  | BoolVal of bool
  | CharVal of char
  | NullpointerVal of typ
  | ArrayVal of typ * value list
  | RecordVal of string * (string * value) list
  | ErrorVal of string
exception CouldNotParseType of string
val canonicType :
  ('a ->
   [< `Found of
        [> `Pointer of 'b | `Record of 'b recordType | `TypeRef of 'a ] as 'b
    | `NotFound ]) ->
  'b -> 'b
val typeOf : value -> typ
val recordDescr : ('a -> string) -> 'a recordType -> string
val typeNameRec : (typ -> string) -> typ -> string
val typeName : typ -> string
val typeDescr : typ -> string
val typeNameExplicit : typ -> string
val valueString : value -> string
val parseType : string -> typ
val parseValue : typ -> string -> value
val defaultValue : typ -> value
type composedType = typ
type integralValue = value
val dequoteEscapeSequence : string -> char
val string2integralValue : string -> value option
type varStorage = RegisterStorage | MemoryStorage
type 'a variable = {
  vname : string;
  typ : 'a;
  vstorage : varStorage;
  vmutable : bool;
  vglobal : bool;
  vlocation : Basics.location option;
}
val validateValue : value -> value
val variable :
  name:string ->
  typ:'a ->
  storage:varStorage ->
  global:bool -> location:Basics.location option -> 'a variable
val varToStringShort : typ variable -> string
val varToString : typ variable -> string
val globalVar :
  name:string -> typ:'a -> location:Basics.location option -> 'a variable
type 'a funcCall = {
  fcname : string;
  fcrettype : composedType;
  fcparams : composedType list;
  fcargs : 'a list;
  fcptr : [ `FuncPtr | `NoFuncPtr ];
  fcvarargs : bool;
}
and label = { lname : string; }
and branch = {
  bcondition : [ `Bool ] variable;
  trueLabel : label;
  falseLabel : label;
}
val funcCallToString : ('a -> string) -> 'a funcCall -> string
val labelToString : label -> string
val branchToString : branch -> string
type 'a flatArgForm =
    [ `Constant of integralValue | `Variable of 'a variable ]
type 'a genericIntrinsic =
    [ `CastIntrinsic of composedType * 'a
    | `GetAddrIntrinsic of composedType variable
    | `GetFieldPointerIntrinsic of 'a * string
    | `LoadIntrinsic of 'a
    | `MallocIntrinsic of composedType * 'a
    | `PtrAddIntrinsic of 'a * 'a
    | `PtrDiffIntrinsic of 'a * 'a
    | `StoreIntrinsic of 'a * 'a ]
type globalVar = {
  gvVar : composedType variable;
  gvInitialValue : value;
  gvDefinitionLocation : Basics.location option;
}
type form =
    [ `AssignVar of composedType variable * form
    | `Branch of branch
    | `CastIntrinsic of composedType * form
    | `Constant of integralValue
    | `DefineVariable of composedType variable * form option
    | `EmbeddedComment of string list
    | `FuncCall of form funcCall
    | `GetAddrIntrinsic of composedType variable
    | `GetFieldPointerIntrinsic of form * string
    | `Jump of label
    | `Label of label
    | `LoadIntrinsic of form
    | `MallocIntrinsic of composedType * form
    | `PtrAddIntrinsic of form * form
    | `PtrDiffIntrinsic of form * form
    | `Return of form
    | `Sequence of form list
    | `StoreIntrinsic of form * form
    | `Variable of composedType variable ]
and func = {
  fname : string;
  rettype : composedType;
  fargs : (string * composedType) list;
  impl : form option;
  cvarargs : bool;
  flocation : Basics.location option;
  fparametric : bool;
}
and toplevelExpr =
    [ `DefineFunc of func
    | `GlobalVar of globalVar
    | `Typedef of string * typ ]
val formToSExpr : form -> Ast2.t
val formToString : form -> string
val funcDeclToString : func -> string
val funcToString : func -> string
val toplevelFormToSExpr :
  [< `DefineFunc of func
   | `GlobalVar of typ variable * value
   | `Typedef of string * typ ] ->
  Ast2.sexpr
val toplevelFormDeclToString : toplevelExpr -> string
val toplevelFormToString : toplevelExpr -> string
val toSingleForm : form list -> form
val isFuncParametric : ('a * typ) list -> bool
val func :
  string ->
  composedType ->
  (string * typ) list -> form option -> Basics.location -> func
val varargFunc :
  string ->
  composedType ->
  (string * typ) list -> form option -> Basics.location -> func
val funcDecl :
  string -> composedType -> (string * typ) list -> Basics.location -> func
val funcDef :
  string ->
  composedType ->
  (string * typ) list -> form -> Basics.location option -> func
type 'a macro = {
  mname : string;
  mtransformFunc : 'a -> Ast2.sexpr -> Ast2.sexpr;
  mdocstring : string;
  mlocation : Basics.location option;
}
val macro :
  string ->
  string -> Basics.location -> ('a -> Ast2.sexpr -> Ast2.sexpr) -> 'a macro
