
open Printf
open Common

let macroVar = "var"
and macroVar2 = "var2"
and macroFunc = "std:base:func"
and macroAssign = "assign"
and macroSequence = "seq"
and macroTypedef = "type"
and macroRecord = "record"
and macroField = "field"
and macroPtr = "ptr"
and macroReturn = "std:base:ret"
and macroLabel = "label"
and macroBranch = "branch"
and macroMacro = "macro"
and macroReplacement = "macroReplace"
and macroFieldptr = "fieldptr"
and macroLoad = "load"
and macroStore = "store"
and macroNullptr = "nullptr"
and macroPtradd = "ptradd"
and macroMalloc = "malloc"
and macroGetaddr = "ptr"
and macroCast = "cast"
and macroInclude = "include"
and macroRest = "postop..."
and macroJuxOp = "opjux"
and macroSeqOp = "opseq"
and macroCallOp = "opcall"
and macroApply = "std:base:apply"
and macroFunCall = "std:base:funcall"
and macroParamType = "op!"

let componentType components componentName =
  try Some( snd (List.find (fun (name, _) -> name = componentName) components) )
  with Not_found -> None

let componentNum components componentName =
  let rec find n = function
    | [] -> raise Not_found
    | (name, _) :: tail when name = componentName -> n
    | _ :: tail -> find (n+1) tail
  in
  find 0 components

include Typesystems.Zomp

type composedType = typ
type integralValue = value

let dequoteEscapeSequence str =
  let numRE = Str.regexp "^\\\\[0-9]\\([0-9][0-9]\\)?$"
  and specialStrings =
    [
      "\\n", '\n';
      "\\t", '\t';
      "\\0", '!';
    ]
  in
  if Str.string_match numRE str 0 then
    let numStr = Str.string_after str 1 in
    let num = int_of_string numStr in
    char_of_int num
  else try
    List.assoc str specialStrings
  with Not_found ->
    failwith (sprintf "Cannot dequote escape sequence %s" str)

let string2integralValue str =
  let dequoteString quoteChar str =
    match Common.dequoteString quoteChar str with
      | `Quoted qstr -> qstr
      | `NotQuoted _ -> raise (Failure (sprintf "dequoteString %c" quoteChar))
  in
  let dequoteChar str =
    let dequoted = dequoteString '\'' str in
    if String.length dequoted = 1 then
      dequoted.[0]
    else
      dequoteEscapeSequence dequoted
  in
  let parseWithSuffix parseF suffix str =
    if Str.string_match (Str.regexp ("\\(.*\\)" ^ suffix)) str 0 then begin
      parseF (Str.matched_group 1 str);
    end else
      raise (Failure "parseWithSuffix")
  in
  let isDigit chr = chr >= '0' && chr <= '9' in
  if isDigit str.[0] &&
    String.contains str '_' &&
    not (str =~ "^[1-9][0-9]?[0-9]?\\(_[0-9][0-9][0-9]\\)+$") then
      None
  else
    tryAll
      [
        lazy( Int32Val (Int32.of_string str) );
        lazy( Int64Val (Int64.of_string str) );
        (* lazy( Int8Val (Int32.of_string str) ); *)
        (* lazy( Int16Val (Int32.of_string str) ); *)
        lazy( FloatVal (parseWithSuffix float_of_string "f" str) );
        lazy( FloatVal (float_of_string str) );
        lazy( BoolVal (bool_of_string str) );
        lazy( CharVal (dequoteChar str) );
        lazy( StringLiteral (dequoteString '"' str) );
        lazy( DoubleVal (parseWithSuffix float_of_string "d" str) );
      ]
      ~onSuccess:some
      ~ifAllFailed:(lazy None)

type varStorage =
  | RegisterStorage
  | MemoryStorage

type 'typ variable = {
  vname :string;
  typ :'typ;
  vstorage :varStorage;
  vmutable :bool;
  vglobal :bool;
}

let rec validateValue = function
  | VoidVal | CharVal _ | BoolVal _
  | Int8Val _ | Int16Val _ | Int32Val _ | Int64Val _
  | NullpointerVal _
  | ErrorVal _
  | StringLiteral _ as value ->
      value
  | FloatVal _ | DoubleVal _ as value ->
      let valueString = Typesystems.Zomp.valueString value in
      Typesystems.Zomp.parseValue (Typesystems.Zomp.typeOf value) valueString
  | ArrayVal (memberType, values) as arrayValue ->
      let validateMemberVal value =
        if (typeOf value <> memberType) then
          failwith (sprintf "Member of %s array had type %s"
                      (typeName memberType)
                      (typeName (typeOf value)));
        ignore (validateValue value);
      in
      List.iter validateMemberVal values;
      arrayValue
  | RecordVal (rname, components) ->
      let validateComponent (name, value) =
        name, validateValue value
      in
      RecordVal (rname, List.map validateComponent components)

let variable ~name ~typ ~storage ~global = {
  vname = name;
  typ = typ;
  vstorage = storage;
  vmutable = false;
  vglobal = global;
}

let varToStringShort var =
  sprintf "%s : %s" var.vname (typeName var.typ)

let varToString var =
  sprintf "%s : %s" var.vname (typeName var.typ)

let globalVar = variable ~storage:MemoryStorage ~global:true

type 'argument funcCall = {
  fcname :string;
  fcrettype :composedType;
  fcparams :composedType list;
  fcargs :'argument list;
  fcptr : [`FuncPtr | `NoFuncPtr];
  fcvarargs :bool;
}
and label = {
  lname :string;
}
and branch = {
  bcondition :[`Bool] variable;
  trueLabel :label;
  falseLabel :label;
}

let funcCallToString argToString fc =
  let argStrings : string list = List.map argToString fc.fcargs in
  sprintf "%s %s" fc.fcname (Common.combine ", " argStrings)

let labelToString l = l.lname

let branchToString b =
  sprintf "%s ? %s : %s" b.bcondition.vname (labelToString b.trueLabel) (labelToString b.falseLabel)

(* TODO: make `Constant + integralValue polymorphic *)
type 'typ flatArgForm = [
| `Variable of 'typ variable
| `Constant of integralValue
]

type 'form genericIntrinsic = [
| `MallocIntrinsic of composedType * 'form
| `GetAddrIntrinsic of composedType variable
| `StoreIntrinsic of 'form * 'form
| `LoadIntrinsic of 'form
| `PtrAddIntrinsic of 'form * 'form (* pointer, int *)
| `GetFieldPointerIntrinsic of 'form * string
| `CastIntrinsic of composedType * 'form
]

type form = [
| composedType flatArgForm
| `Sequence of form list
| `DefineVariable of composedType variable * form option
| `FuncCall of form funcCall
| `AssignVar of composedType variable * form
| `Return of form
| `Jump of label
| `Branch of branch
| `Label of label
| form genericIntrinsic
| `EmbeddedComment of string list
]
and func = {
  fname :string;
  rettype :composedType;
  fargs :(string * composedType) list;
  impl :form option;
  cvarargs :bool;
  fparametric :bool;
}
and toplevelExpr = [
| `GlobalVar of composedType variable * value
| `DefineFunc of func
| `Typedef of string * typ
]

let rec formToSExpr : form -> Ast2.t = function
  | `Variable var ->
      Ast2.simpleExpr "Variable" [var.vname]
  | `Constant c ->
      Ast2.simpleExpr "Constant" [valueString c]
  | `Sequence forms ->
      Ast2.seqExpr (List.map formToSExpr forms)
  | `DefineVariable (var, formOption) ->
      Ast2.expr "DefVar" [Ast2.idExpr (typeName var.typ);
                          Ast2.idExpr var.vname;
                          match formOption with
                            | Some form -> formToSExpr form
                            | None -> Ast2.idExpr "undefined"]
  | `FuncCall fc ->
      Ast2.expr fc.fcname (List.map formToSExpr fc.fcargs)
  | `AssignVar (var, form) ->
      Ast2.expr "Assign" [Ast2.idExpr var.vname; formToSExpr form]
  | `Return form ->
      Ast2.expr "Return" [formToSExpr form]
  | `Jump label ->
      Ast2.simpleExpr "Jump" [labelToString label]
  | `Branch b ->
      Ast2.simpleExpr "Branch" [b.bcondition.vname;
                                labelToString b.trueLabel;
                                labelToString b.falseLabel]
  | `Label l ->
      Ast2.simpleExpr "Label" [labelToString l]
  | `MallocIntrinsic (typ, form) ->
      Ast2.expr "Malloc" [Ast2.idExpr (typeName typ); formToSExpr form]
  | `GetAddrIntrinsic var ->
      Ast2.simpleExpr "AddressOf" [var.vname]
  | `StoreIntrinsic (ptr, value) ->
      Ast2.expr "Store" [formToSExpr ptr; formToSExpr value]
  | `LoadIntrinsic (ptr) ->
      Ast2.expr "Load" [formToSExpr ptr]
  | `PtrAddIntrinsic (ptr, offset) ->
      Ast2.expr "PtrAdd" [formToSExpr ptr; formToSExpr offset]
  | `GetFieldPointerIntrinsic (record, fieldName) ->
      Ast2.expr "GetFieldPtr" [formToSExpr record; Ast2.idExpr fieldName]
  | `CastIntrinsic (typ, expr) ->
      Ast2.expr "Cast" [Ast2.idExpr (typeName typ); formToSExpr expr]
  | `EmbeddedComment strings ->
      let addQuotes str = "\"" ^ str ^ "\"" in
      Ast2.expr "Comment" (List.map (Ast2.idExpr ++ addQuotes) strings)

let rec formToString : form -> string = function
  | `Variable var -> sprintf "Variable %s" (varToString var)
  | `Constant c -> sprintf "Constant %s" (valueString c)
  | `Sequence s ->
      let strings = List.map formToString s in
      sprintf "Sequence(\n%s\n)" (Common.combine "\n  " strings)
  | `DefineVariable (var, form) ->
      sprintf "DefineVar(%s = %s)" (varToStringShort var) (match form with Some form -> formToString form | None -> "undef")
  | `FuncCall fc ->
      sprintf "FuncCall(%s)" (funcCallToString formToString fc)
  | `AssignVar (var, form) ->
      sprintf "AssignVar(%s, %s)" (varToStringShort var) (formToString form)
  | `Return form ->
      sprintf "Return( %s )" (formToString form)
  | `Jump label ->
      sprintf "Jump( %s )" (labelToString label)
  | `Branch b ->
      sprintf "Branch( %s )" (branchToString b)
  | `Label l ->
      labelToString l
  | `MallocIntrinsic (typ, form) -> sprintf "malloc %s x %s" (typeName typ) (formToString form)
  | `GetAddrIntrinsic var -> sprintf "GetAddr (%s)" (varToString var)
  | `StoreIntrinsic (ptr, value) -> sprintf "Store (%s, %s)" (formToString ptr) (formToString value)
  | `LoadIntrinsic (ptr) -> sprintf "Load (%s)" (formToString ptr)
  | `PtrAddIntrinsic (ptr, offset) -> sprintf "PtrAdd (%s, %s)" (formToString ptr) (formToString offset)
  | `GetFieldPointerIntrinsic (record, fieldName) -> sprintf "GetField (%s, %s)" (formToString record) fieldName
  | `CastIntrinsic (typ, expr) -> sprintf "Cast (%s, %s)" (typeName typ) (formToString expr)
  | `EmbeddedComment strings ->
      sprintf "Comments ('%s')" (Common.combine "', '" strings)

let funcDeclToString func =
  let argToString (name, typ) = name ^ " :" ^ typeName typ in
  let argStrings = List.map argToString func.fargs in
  sprintf "%s(%s) :%s"
    func.fname
    (Common.combine ", " argStrings)
    (typeName func.rettype)

let funcToString func =
  funcDeclToString func ^ (match func.impl with
       | Some form -> " = \n" ^ formToString form
       | None -> "")

let toplevelFormToSExpr =
  let id = Ast2.idExpr in
  function
    | `GlobalVar (var, initialValue) ->
        Ast2.simpleExpr "GlobalVar" [typeName var.typ; var.vname; valueString initialValue]
    | `DefineFunc func ->
        let implExpr = match func.impl with
          | Some expr -> [formToSExpr expr]
          | None -> []
        in
        let argToSExpr (name, typ) = Ast2.simpleExpr (typeName typ) [name] in
        Ast2.expr "Func" ([id (typeName func.rettype);
                           id func.fname;
                           Ast2.expr "opcall" (List.map argToSExpr func.fargs)]
                          @ implExpr)
    | `Typedef (name, typ) ->
        Ast2.simpleExpr "Typedef" [name; typeName typ]

let toplevelFormDeclToString = function
  | `GlobalVar (var, initialValue) ->
      sprintf "var %s = %s" (varToString var) (valueString initialValue)
  | `DefineFunc func ->
      sprintf "func %s" (funcDeclToString func)
  | `Typedef (name, typ) ->
      sprintf "type %s = %s" name (typeDescr typ)

let toplevelFormToString = function
  | `GlobalVar (var, initialValue) ->
      sprintf "var %s = %s" (varToString var) (valueString initialValue)
  | `DefineFunc func ->
      sprintf "func %s" (funcToString func)
  | `Typedef (name, typ) ->
      sprintf "type %s = %s" name (typeName typ)

let toSingleForm formlist =
  match formlist with
    | [(singleForm :form)] -> singleForm
    | sequence -> `Sequence sequence

let isFuncParametric args =
  List.exists (isTypeParametric ++ snd) args

let func name rettype args impl = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = impl;
  cvarargs = false;
  fparametric = isFuncParametric args;
}

let varargFunc name rettype args impl = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = impl;
  cvarargs = true;
  fparametric = isFuncParametric args;
}

let funcDecl name rettype args = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = None;
  cvarargs = false;
  fparametric = isFuncParametric args;
}

let funcDef name rettype args impl = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = Some impl;
  cvarargs = false;
  fparametric = isFuncParametric args;
}

type 'bindings macro = {
  mname :string;
  mtransformFunc : 'bindings -> Ast2.sexpr -> Ast2.sexpr;
  mdocstring :string;
}

