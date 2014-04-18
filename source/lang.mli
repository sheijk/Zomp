(** The AST for std:base language. **)

(** The id for std:base constructs. *)
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
val macroConstructor : string

val componentType : ('a * 'b) list -> 'a -> 'b option
val componentNum : ('a * 'b) list -> 'a -> int

type typ = Typesystems.Zomp.typ
type 'a recordType = 'a Typesystems.Zomp.recordType
type functionType = Typesystems.Zomp.functionType
type 'a parameterizableType = 'a Typesystems.Zomp.parameterizableType

val dequoteEscapeSequence : string -> char
val string2integralValue : string -> Typesystems.Zomp.value option
type varStorage = RegisterStorage | MemoryStorage
type 'a variable = {
  vname : string;
  typ : 'a;
  vstorage : varStorage;
  vmutable : bool;
  vglobal : bool;
  vlocation : Basics.location option;
}
val validateValue : Typesystems.Zomp.value -> Typesystems.Zomp.value
val variable :
  name:string ->
  typ:'a ->
  storage:varStorage ->
  global:bool -> location:Basics.location option -> 'a variable
val varToStringShort : Typesystems.Zomp.typ variable -> string
val varToString : Typesystems.Zomp.typ variable -> string
val globalVar :
  name:string -> typ:'a -> location:Basics.location option -> 'a variable
type 'a funcCall = {
  fcname : string;
  fcrettype : typ;
  fcparams : typ list;
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
    [ `Constant of Typesystems.Zomp.value | `Variable of 'a variable ]
type 'a genericIntrinsic =
    [ `CastIntrinsic of typ * 'a
    | `GetAddrIntrinsic of typ variable
    | `GetFieldPointerIntrinsic of 'a * string
    | `LoadIntrinsic of 'a
    | `MallocIntrinsic of typ * 'a
    | `PtrAddIntrinsic of 'a * 'a
    | `PtrDiffIntrinsic of 'a * 'a
    | `StoreIntrinsic of 'a * 'a ]
type globalVar = {
  gvVar : typ variable;
  gvInitialValue : Typesystems.Zomp.value;
  gvDefinitionLocation : Basics.location option;
}
type form =
    [ `AssignVar of typ variable * form
    | `Branch of branch
    | `CastIntrinsic of typ * form
    | `Constant of Typesystems.Zomp.value
    | `DefineVariable of typ variable * form option
    | `EmbeddedComment of string list
    | `FuncCall of form funcCall
    | `GetAddrIntrinsic of typ variable
    | `GetFieldPointerIntrinsic of form * string
    | `Jump of label
    | `Label of label
    | `LoadIntrinsic of form
    | `MallocIntrinsic of typ * form
    | `PtrAddIntrinsic of form * form
    | `PtrDiffIntrinsic of form * form
    | `Return of form
    | `Sequence of form list
    | `StoreIntrinsic of form * form
    | `Variable of typ variable ]
and func = {
  fname : string;
  rettype : typ;
  fargs : (string * typ) list;
  impl : form option;
  cvarargs : bool;
  flocation : Basics.location option;
  fparametric : bool;
}
and toplevelExpr =
    [ `DefineFunc of func
    | `GlobalVar of globalVar
    | `Typedef of string * Typesystems.Zomp.typ ]
val formToSExpr : form -> Ast2.t
val formToString : form -> string
val funcDeclToString : func -> string
val funcToString : func -> string
val toplevelFormToSExpr :
  [< `DefineFunc of func
   | `GlobalVar of Typesystems.Zomp.typ variable * Typesystems.Zomp.value
   | `Typedef of string * Typesystems.Zomp.typ ] ->
  Ast2.sexpr
val toplevelFormDeclToString : toplevelExpr -> string
val toplevelFormToString : toplevelExpr -> string
val toplevelFormLocation : toplevelExpr -> Basics.location
val toSingleForm : form list -> form
val isFuncParametric : ('a * Typesystems.Zomp.typ) list -> bool
val func :
  string ->
  typ ->
  (string * Typesystems.Zomp.typ) list ->
  form option -> Basics.location -> func
val varargFunc :
  string ->
  typ ->
  (string * Typesystems.Zomp.typ) list ->
  form option -> Basics.location -> func
val funcDecl :
  string ->
  typ ->
  (string * Typesystems.Zomp.typ) list -> Basics.location -> func
val funcDef :
  string ->
  typ ->
  (string * Typesystems.Zomp.typ) list ->
  form -> Basics.location option -> func
type 'a macro = {
  mname : string;
  mtransformFunc : 'a -> Ast2.sexpr -> Ast2.sexpr;
  mdocstring : string;
  mlocation : Basics.location option;
}
val macro :
  string ->
  string -> Basics.location -> ('a -> Ast2.sexpr -> Ast2.sexpr) -> 'a macro
