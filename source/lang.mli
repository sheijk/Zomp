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

(** (Nested) Expressions *)

type typ = Types.typ
val string2integralValue : string -> Types.value option

type varStorage = RegisterStorage | MemoryStorage
type +'typ variable = private {
  vname : string;
  typ : 'typ;
  vstorage : varStorage;
  vmutable : bool;
  vglobal : bool;
  vlocation : Basics.location option;
}

val variable :
  name:string ->
  typ:'a ->
  storage:varStorage ->
  global:bool -> location:Basics.location option -> 'a variable

val varWithType : 'typA variable -> 'typB -> 'typB variable

val varToStringShort : Types.typ variable -> string
val varToString : Types.typ variable -> string

val globalVar : name:string -> typ:'a -> location:Basics.location option -> 'a variable

type +'arg funcCall = private {
  fcname : string;
  fcrettype : typ;
  fcparams : typ list;
  fcargs : 'arg list;
  fcptr : [ `FuncPtr | `NoFuncPtr ];
  fcvarargs : bool;
}

val funcCall :
  name:string ->
  rettype:typ ->
  params:typ list ->
  args:'arg list ->
  ptr:[`FuncPtr | `NoFuncPtr] ->
  varargs:bool ->
  'arg funcCall

val funcCallToString : ('a -> string) -> 'a funcCall -> string
val changeFuncCallArgs : 'a funcCall -> 'b list -> 'b funcCall

type label = private { lname : string; }
val label : string -> label
val labelToString : label -> string

type branch = private {
  bcondition : [ `Bool ] variable;
  trueLabel : label;
  falseLabel : label;
}
val branch : [`Bool] variable -> label -> label -> branch
val branchToString : branch -> string

type 'a genericIntrinsic =
    [ `CastIntrinsic of typ * 'a
    | `GetAddrIntrinsic of typ variable
    | `GetFieldPointerIntrinsic of 'a * string
    | `LoadIntrinsic of 'a
    | `MallocIntrinsic of typ * 'a
    | `PtrAddIntrinsic of 'a * 'a
    | `PtrDiffIntrinsic of 'a * 'a
    | `StoreIntrinsic of 'a * 'a ]

type form =
    [ `AssignVar of typ variable * form
    | `Branch of branch
    | `CastIntrinsic of typ * form
    | `Constant of Types.value
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

(** Global forms *)

type globalVar = private {
  gvVar : typ variable;
  gvInitialValue : Types.value;
  gvDefinitionLocation : Basics.location option;
}
val globalVarDef : var:typ variable -> initial:Types.value -> location:Basics.location option -> globalVar

type func = private {
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
    | `Typedef of string * Types.typ ]

val formToSExpr : form -> Ast2.t
val formToString : form -> string
val funcDeclToString : func -> string
val funcToString : func -> string
val toplevelFormToSExpr :
  [< `DefineFunc of func
   | `GlobalVar of Types.typ variable * Types.value
   | `Typedef of string * Types.typ ] ->
  Ast2.sexpr
val toplevelFormDeclToString : toplevelExpr -> string
val toplevelFormToString : toplevelExpr -> string
val toplevelFormLocation : toplevelExpr -> Basics.location

val toSingleForm : form list -> form

val func :
  string ->
  typ ->
  (string * Types.typ) list ->
  form option -> Basics.location -> func
val varargFunc :
  string ->
  typ ->
  (string * Types.typ) list ->
  form option -> Basics.location -> func
val funcDecl :
  string ->
  typ ->
  (string * Types.typ) list -> Basics.location -> func
val funcDef :
  string ->
  typ ->
  (string * Types.typ) list ->
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
