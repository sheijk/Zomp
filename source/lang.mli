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
val macroSizeof : string
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
  (** Location of the variable's definition *)
  vlocation : Basics.location option;
  (** A local index that's unique in it's function. -1 for globals. Indices for
   local variables start at 0 and are consecutive in the function. *)
  mutable vlocalIndex :int;
}

val setLocalIndex : 'a variable -> int -> unit

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
  bcondition : typ variable;
  trueLabel : label;
  falseLabel : label;
}
val branch : typ variable -> label -> label -> branch
val branchToString : branch -> string

type formInfo
val formInfo : Basics.location -> formInfo
val formInfoFromExpr : Ast2.t -> formInfo


type 'a genericIntrinsic =
    [ `CastIntrinsic of formInfo * typ * 'a
    | `GetAddrIntrinsic of formInfo * typ variable
    | `GetFieldPointerIntrinsic of formInfo * 'a * string
    | `LoadIntrinsic of formInfo * 'a
    | `SizeofIntrinsic of formInfo * typ
    | `PtrAddIntrinsic of formInfo * 'a * 'a
    | `PtrDiffIntrinsic of formInfo * 'a * 'a
    | `StoreIntrinsic of formInfo * 'a * 'a ]

type form =
    [ `AssignVar of formInfo * typ variable * form
    | `Branch of formInfo * branch
    | `Constant of formInfo * Types.value
    | `DefineVariable of formInfo * typ variable * form option
    | `EmbeddedComment of formInfo * string list
    | `FuncCall of formInfo * form funcCall
    | `Jump of formInfo * label
    | `Label of formInfo * label
    | `Return of formInfo * form
    | `Sequence of formInfo * (form list)
    | `Variable of formInfo * (typ variable)
    | form genericIntrinsic]

val flocation : form -> Basics.location
val info : form -> formInfo

val sequence : form list -> form
val defineVariable : typ variable -> form option -> form

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
  fargs : typ variable list;
  impl : form option;
  cvarargs : bool;
  flocation : Basics.location option;
  fparametric : bool;
  (** 1 + the highest vlocalIndex of all variables defined in this function. *)
  mutable flocalVariableCount :int;
}
and toplevelExpr =
    [ `DefineFunc of func
    | `GlobalVar of globalVar
    | `Typedef of string * Types.typ ]

val formToString : form -> string
val formToAst : form -> Ast2.t
val funcDeclToString : func -> string
val funcToString : func -> string
val toplevelFormDeclToString : toplevelExpr -> string
val toplevelFormToString : toplevelExpr -> string
val toplevelFormLocation : toplevelExpr -> Basics.location
val toplevelFormToAst : toplevelExpr -> Ast2.t

val toSingleForm : form list -> form

val func :
  string ->
  typ ->
  typ variable list ->
  form option ->
  Basics.location ->
  func
val varargFunc :
  string ->
  typ ->
  typ variable list ->
  form option ->
  Basics.location ->
  func
val funcDecl :
  string ->
  typ ->
  typ variable list ->
  Basics.location ->
  func
val funcDef :
  string ->
  typ ->
  typ variable list ->
  form ->
  Basics.location option ->
  func
val funcParam : string -> typ -> typ variable

val setLocalVariableCount : func -> int -> unit

type 'a macro = {
  mname : string;
  mtransformFunc : 'a -> Ast2.sexpr -> Ast2.sexpr;
  mdocstring : string;
  mlocation : Basics.location option;
}
val macro :
  string ->
  string -> Basics.location -> ('a -> Ast2.sexpr -> Ast2.sexpr) -> 'a macro

(** Will return a string which can be feed back into the parser to produce the
same AST (not preserving source locations). *)
val astToSource : int -> [`WrapNone|`WrapParen] -> Ast2.t -> string

