
open Printf
open Common

open Types

let macroVar = "var"
and macroVar2 = "var2"
and macroFunc = "std:base:func"
and macroAssign = "assign"
and macroSequence = "seq"
and macroTypedef = "type"
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
and macroPtrDiff = "ptrdiff"
and macroSizeof = "std:base:sizeof"
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
and macroError = "std:base:error"
and macroLinkCLib = "zmp:compiler:linkclib"
and macroConstructor = "std:base:constructorCall"

type typ = Types.typ
type 'a recordType = 'a Types.recordType
type functionType = Types.functionType
type 'a parameterizableType = 'a Types.parameterizableType

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
    failwith (sprintf "cannot dequote escape sequence %s" str)

(** TODO: check if this function is sane or it's usage sites should be changed *)
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
  vlocation :Basics.location option;
  mutable vlocalIndex :int;
}

let setLocalIndex var index =
  var.vlocalIndex <- index

let varLoc var = someOrDefault var.vlocation Basics.fakeLocation

let rec validateValue = function
  | VoidVal | CharVal _ | BoolVal _
  | Int8Val _ | Int16Val _ | Int32Val _ | Int64Val _
  | NullpointerVal _
  | ErrorVal _
  | StringLiteral _ as value ->
      value
  | FloatVal _ | DoubleVal _ as value ->
      let valueString = Types.valueString value in
      Types.parseValue (Types.typeOf value) valueString
  | ArrayVal (memberType, values) as arrayValue ->
      let validateMemberVal value =
        if (typeOf value <> memberType) then
          failwith (sprintf "member of %s array had type %s"
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

let variable ~name ~typ ~storage ~global ~location = {
  vname = name;
  typ = typ;
  vstorage = storage;
  vmutable = false;
  vglobal = global;
  vlocation = location;
  vlocalIndex = -1;
}

let varWithType var newType = { var with typ = newType }

let varToStringShort var =
  sprintf "%s : %s" var.vname (typeName var.typ)

let varToString var =
  sprintf "%s : %s" var.vname (typeName var.typ)

let globalVar = variable ~storage:MemoryStorage ~global:true

type 'argument funcCall = {
  fcname :string;
  fcrettype :typ;
  fcparams :typ list;
  fcargs :'argument list;
  fcptr : [`FuncPtr | `NoFuncPtr];
  fcvarargs :bool;
}

let funcCall ~name ~rettype ~params ~args ~ptr ~varargs =
  {
    fcname = name;
    fcrettype = rettype;
    fcparams = params;
    fcargs = args;
    fcptr = ptr;
    fcvarargs = varargs;
  }

let changeFuncCallArgs call newArgs = { call with fcargs = newArgs }

let funcCallToString argToString fc =
  let argStrings : string list = List.map argToString fc.fcargs in
  sprintf "%s %s" fc.fcname (Common.combine ", " argStrings)

type label = {
  lname :string;
}

let label lname = { lname }
let labelToString l = l.lname

type branch = {
  bcondition :typ variable;
  trueLabel :label;
  falseLabel :label;
}

let branch bcondition trueLabel falseLabel =
  assert (bcondition.typ = `Bool);
  { bcondition; trueLabel; falseLabel; }

let branchToString b =
  sprintf "%s ? %s : %s" b.bcondition.vname (labelToString b.trueLabel) (labelToString b.falseLabel)

type formInfo = {
  formLoc : Basics.location;
}

(* TODO: make `Constant + integralValue polymorphic *)
type 'typ flatArgForm = [
| `Variable of 'typ variable
| `Constant of Types.value
]

type 'a genericIntrinsic =
  [ `CastIntrinsic of formInfo * typ * 'a
  | `GetAddrIntrinsic of formInfo * typ variable
  | `GetFieldPointerIntrinsic of formInfo * 'a * string
  | `LoadIntrinsic of formInfo * 'a
  | `SizeofIntrinsic of formInfo * typ
  | `PtrAddIntrinsic of formInfo * 'a * 'a
  | `PtrDiffIntrinsic of formInfo * 'a * 'a
  | `StoreIntrinsic of formInfo * 'a * 'a ]

type globalVar = {
  gvVar :typ variable;
  gvInitialValue :value;
  gvDefinitionLocation :Basics.location option;
}
let globalVarDef ~var ~initial ~location = 
  {
    gvVar = var;
    gvInitialValue = initial;
    gvDefinitionLocation = location;
  }

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
and func = {
  fname :string;
  rettype :typ;
  fargs :typ variable list;
  impl :form option;
  cvarargs :bool;
  flocation :Basics.location option;
  fparametric :bool;
  mutable flocalVariableCount :int;
}
and toplevelExpr = [
| `GlobalVar of globalVar
| `DefineFunc of func
| `Typedef of string * typ
]

let info : form -> formInfo = function
  | `CastIntrinsic (info, _, _)
  | `GetAddrIntrinsic (info, _)
  | `GetFieldPointerIntrinsic (info, _, _)
  | `LoadIntrinsic (info, _)
  | `SizeofIntrinsic (info, _)
  | `PtrAddIntrinsic (info, _, _)
  | `PtrDiffIntrinsic (info, _, _)
  | `StoreIntrinsic (info, _, _)
  | `AssignVar (info, _, _)
  | `Branch (info, _)
  | `Constant (info, _)
  | `DefineVariable (info, _, _)
  | `EmbeddedComment (info, _)
  | `FuncCall (info, _)
  | `Jump (info, _)
  | `Label (info, _)
  | `Return (info, _)
  | `Sequence (info, _)
  | `Variable (info, _)
    -> info

let formInfo formLoc = { formLoc }
let formInfoFromExpr expr = formInfo $ Ast2.location expr

let flocation form =
  let info = info form in
  info.formLoc

let inferFormInfo : form list -> formInfo = function
  | [] -> { formLoc = Basics.fakeLocation }
  | hd :: _ -> info hd

let sequence forms = `Sequence (inferFormInfo forms, forms)
let defineVariable var initForm =
  `DefineVariable (formInfo (varLoc var), var, initForm)

let rec formToString : form -> string = function
  | `Variable (_, var) -> sprintf "Variable %s" (varToString var)
  | `Constant (_, c) -> sprintf "Constant %s" (valueString c)
  | `Sequence (_, s) ->
      let strings = List.map formToString s in
      sprintf "Sequence(\n%s\n)" (Common.combine "\n  " strings)
  | `DefineVariable (_, var, form) ->
      sprintf "DefineVar(%s = %s)" (varToStringShort var) (match form with Some form -> formToString form | None -> "undef")
  | `FuncCall (_, fc) ->
      sprintf "FuncCall(%s)" (funcCallToString formToString fc)
  | `AssignVar (_, var, form) ->
      sprintf "AssignVar(%s, %s)" (varToStringShort var) (formToString form)
  | `Return (_, form) ->
      sprintf "Return( %s )" (formToString form)
  | `Jump (_, label) ->
      sprintf "Jump( %s )" (labelToString label)
  | `Branch (_, b) ->
      sprintf "Branch( %s )" (branchToString b)
  | `Label (_, l) ->
      labelToString l
  | `SizeofIntrinsic (_, typ) -> sprintf "%s %s" macroSizeof (typeName typ)
  | `GetAddrIntrinsic (_, var) -> sprintf "GetAddr (%s)" (varToString var)
  | `StoreIntrinsic (_, ptr, value) -> sprintf "Store (%s, %s)" (formToString ptr) (formToString value)
  | `LoadIntrinsic (_, ptr) -> sprintf "Load (%s)" (formToString ptr)
  | `PtrAddIntrinsic (_, ptr, offset) -> sprintf "PtrAdd (%s, %s)" (formToString ptr) (formToString offset)
  | `PtrDiffIntrinsic (_, lhs, rhs) ->
    sprintf "PtrDiff (%s, %s)" (formToString lhs) (formToString rhs)
  | `GetFieldPointerIntrinsic (_, record, fieldName) -> sprintf "GetField (%s, %s)" (formToString record) fieldName
  | `CastIntrinsic (_, typ, expr) -> sprintf "Cast (%s, %s)" (typeName typ) (formToString expr)
  | `EmbeddedComment (_, strings) ->
      sprintf "Comments ('%s')" (Common.combine "', '" strings)

let rec formKindToString : form -> string = function
  | `Variable _ -> "Variable"
  | `Constant _ -> "Constant"
  | `Sequence _ -> "Sequence"
  | `DefineVariable _ -> "DefineVariable"
  | `FuncCall _ -> "FuncCall"
  | `AssignVar _ -> "AssignVar"
  | `Return _ -> "Return"
  | `Jump _ -> "Jump"
  | `Branch _ -> "Branch"
  | `Label _ -> "Jump"
  | `SizeofIntrinsic _ -> "SizeofIntrinsic"
  | `GetAddrIntrinsic _ -> "GetAddrIntrinsic"
  | `StoreIntrinsic _ -> "StoreIntrinsic"
  | `LoadIntrinsic _ -> "LoadIntrinsic"
  | `PtrAddIntrinsic _ -> "PtrAddIntrinsic"
  | `PtrDiffIntrinsic _ -> "PtrDiffIntrinsic"
  | `GetFieldPointerIntrinsic _ -> "GetFieldPointerIntrinsic"
  | `CastIntrinsic _ -> "CastIntrinsic"
  | `EmbeddedComment _ -> "EmbeddedComment"

let rec typeToAst (format : [`Short | `Long]) : Types.typ -> Ast2.t = function
  | (#integralType | `ErrorType _) as t ->
     Ast2.exprNoLoc (Types.typeName t) []
  | `Pointer target ->
     Ast2.exprNoLoc "postop*" [typeToAst `Short target]
  | `TypeRef name ->
     Ast2.exprNoLoc name []
  | `Record record ->
     if format = `Long then
       Ast2.exprNoLoc macroSeqOp @@ List.map (fun (fieldName, fieldType) ->
                                              Ast2.exprNoLoc macroJuxOp
                                                             [typeToAst `Short fieldType;
                                                              Ast2.exprNoLoc fieldName []])
                                             record.fields
     else
       Ast2.exprNoLoc record.rname []
  | `Array (elementType, size) ->
     Ast2.exprNoLoc "postop[]" [typeToAst `Short elementType; Ast2.exprNoLoc (sprintf "%d" size) []]
  | `Function func ->
     Ast2.exprNoLoc macroCallOp (typeToAst `Short func.returnType :: List.map (typeToAst `Short) func.argTypes)
  | `ParametricType typ ->
     Ast2.exprNoLoc macroParamType [typeToAst `Short (typ :> typ); Ast2.exprNoLoc "T" []]
  | `TypeParam ->
     Ast2.exprNoLoc "T" []

let rec formToAst form =
  let loc = flocation form in
  let fromVar var =
    Ast2.idExprLoc (someOrDefault var.vlocation loc) var.vname
  in

  match form with
    | `Variable (_, var) ->
       fromVar var
    (* TODO: add 'std:base:constant type value' *)
    | `Constant (_, c) ->
       Ast2.idExprLoc loc @@ valueString c
    | `Sequence (_, args) ->
       Ast2.exprLoc loc macroSequence @@ List.map formToAst args
    | `DefineVariable (_, var, init) ->
       Ast2.exprLoc loc macroVar @@
         [Ast2.idExprLoc loc (typeName var.typ);
          fromVar var]
         @ (match init with Some initForm -> [formToAst initForm] | _ -> [])
    | `AssignVar (_, var, value) ->
       Ast2.exprLoc loc macroAssign [fromVar var; formToAst value]
    | `Return (_, value) ->
       Ast2.exprLoc loc macroReturn [formToAst value]
    | `Jump (_, label) ->
       Ast2.exprLoc loc macroBranch [Ast2.idExprLoc loc label.lname]
    | `Branch (_, br) ->
       Ast2.exprLoc loc macroBranch @@
         [fromVar br.bcondition;
          Ast2.idExprLoc loc br.trueLabel.lname;
          Ast2.idExprLoc loc br.falseLabel.lname]
    | `Label (_, label) ->
       Ast2.exprLoc loc macroLabel [Ast2.idExprLoc loc label.lname]
    | `FuncCall (_, call) ->
       Ast2.exprLoc loc macroCallOp
                    (Ast2.idExprLoc loc call.fcname ::
                       List.map formToAst call.fcargs)

    | `CastIntrinsic (_, targetType, form) ->
       Ast2.exprLoc loc macroCast [typeToAst `Short targetType; formToAst form]
    | `GetAddrIntrinsic (_, var) ->
       Ast2.exprLoc loc macroGetaddr [Ast2.idExprLoc (someOrDefault var.vlocation Basics.fakeLocation) var.vname]
    | `GetFieldPointerIntrinsic (_, form, fieldName) ->
       Ast2.exprLoc loc macroFieldptr [formToAst form; Ast2.exprNoLoc fieldName []]
    | `LoadIntrinsic (_, form) ->
       Ast2.exprLoc loc macroLoad [formToAst form]
    | `SizeofIntrinsic (_, typ) ->
       Ast2.exprLoc loc macroSizeof [typeToAst `Short typ]
    | `PtrAddIntrinsic (_, lhs, rhs) ->
       Ast2.exprLoc loc macroPtradd [formToAst lhs; formToAst rhs]
    | `PtrDiffIntrinsic (_, lhs, rhs) ->
       Ast2.exprLoc loc macroPtrDiff [formToAst lhs; formToAst rhs]
    | `StoreIntrinsic (_, ptrForm, valueForm) ->
       Ast2.exprLoc loc macroStore [formToAst ptrForm; formToAst valueForm]

    | `EmbeddedComment _ ->
       let error = sprintf "embedded comment at %s" @@ Basics.locationToString loc in
       failwith error

let toplevelFormToAst form =
  match form with
    | `DefineFunc func ->
       let loc = someOrDefault func.flocation Basics.fakeLocation in
       let nameAndArgs =
         let argToAst var =
           Ast2.exprLoc loc macroJuxOp [typeToAst `Short var.typ; Ast2.idExprLoc loc var.vname]
         in
         let nameAst =
           if func.fparametric then
             Ast2.exprLoc loc macroParamType [Ast2.idExprLoc loc func.fname; Ast2.idExprLoc loc "T"]
           else
             Ast2.idExprLoc loc func.fname
         in
         Ast2.exprLoc loc macroCallOp @@ nameAst :: List.map argToAst func.fargs
       in
       Ast2.exprLoc loc macroFunc @@
         [typeToAst `Short func.rettype;
          nameAndArgs]
         @ (match func.impl with Some body -> [formToAst body] | _ -> [])

    | `GlobalVar gvar ->
       let loc = someOrDefault gvar.gvDefinitionLocation Basics.fakeLocation in
       Ast2.exprLoc loc macroVar @@
         [typeToAst `Short gvar.gvVar.typ;
          Ast2.idExprLoc loc gvar.gvVar.vname;
          Ast2.idExprLoc loc @@ valueString gvar.gvInitialValue]

    | `Typedef (name, `ParametricType (`Record _ as typ)) ->
       let loc = Basics.fakeLocation in
       Ast2.exprLoc loc macroTypedef
                    [Ast2.exprLoc loc macroParamType [Ast2.idExprLoc loc name; Ast2.idExprLoc loc "T"];
                     typeToAst `Long typ]

    | `Typedef (name, typ) ->
       let loc = Basics.fakeLocation in
       Ast2.exprLoc loc macroTypedef [Ast2.idExprLoc loc name; typeToAst `Long typ]

let funcDeclToString func =
  let argToString var = var.vname ^ " :" ^ typeName var.typ in
  let argStrings = List.map argToString func.fargs in
  sprintf "%s(%s) :%s"
    func.fname
    (Common.combine ", " argStrings)
    (typeName func.rettype)

let funcToString func =
  funcDeclToString func ^ (match func.impl with
       | Some form -> " = \n" ^ formToString form
       | None -> "")

let toplevelFormDeclToString : toplevelExpr -> string = function
  | `GlobalVar gvar ->
      sprintf "var %s = %s" (varToString gvar.gvVar) (valueString gvar.gvInitialValue)
  | `DefineFunc func ->
      sprintf "func %s" (funcDeclToString func)
  | `Typedef (name, typ) ->
      sprintf "type %s = %s" name (typeDescr typ)

let toplevelFormToString : toplevelExpr -> string = function
  | `GlobalVar gvar ->
      sprintf "var %s = %s" (varToString gvar.gvVar) (valueString gvar.gvInitialValue)
  | `DefineFunc func ->
      sprintf "func %s" (funcToString func)
  | `Typedef (name, typ) ->
      sprintf "type %s = %s" name (typeName typ)

let toplevelFormLocation : toplevelExpr -> Basics.location = function
  | `GlobalVar { gvDefinitionLocation = Some location }
  | `DefineFunc { flocation = Some location } ->
    location
  | `GlobalVar { gvDefinitionLocation = None }
  | `DefineFunc { flocation = None }
  | `Typedef _ ->
    Basics.fakeLocation

let toSingleForm formlist =
  match formlist with
    | [(singleForm :form)] -> singleForm
    | forms -> sequence forms

let isFuncParametric args =
  List.exists (fun var -> isTypeParametric var.typ) args

let func name rettype args impl location = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = impl;
  flocation = Some location;
  cvarargs = false;
  fparametric = isFuncParametric args;
  flocalVariableCount = -1;
}

let varargFunc name rettype args impl location = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = impl;
  flocation = Some location;
  cvarargs = true;
  fparametric = isFuncParametric args;
  flocalVariableCount = -1;
}

let funcDecl name rettype args location = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = None;
  flocation = Some location;
  cvarargs = false;
  fparametric = isFuncParametric args;
  flocalVariableCount = -1;
}

let funcDef name rettype args impl location = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = Some impl;
  flocation = location;
  cvarargs = false;
  fparametric = isFuncParametric args;
  flocalVariableCount = -1;
}

let funcParam name typ =
  variable
    ~name ~typ
    (** all parameters are copied into a local var by backend genllvm *)
    ~storage:MemoryStorage
    ~global:false
    ~location:None

let setLocalVariableCount func count =
  func.flocalVariableCount <- count

type 'bindings macro = {
  mname :string;
  mtransformFunc : 'bindings -> Ast2.sexpr -> Ast2.sexpr;
  mdocstring :string;
  mlocation :Basics.location option;
}

let macro name doc location func =
  { mname = name; mtransformFunc = func; mdocstring = doc; mlocation = Some location }

let rec astToSource indentLevel nested ast =
  let isSeq id =
    id = macroSeqOp || id = macroSequence
  in
  let combineJux args f =
    let addSpaceUnlessSeq ast =
      if isSeq ast.Ast2.id then
        f ast
      else
        " " ^ f ast
    in
    match args with
      | first :: remArgs ->
         Common.combine "" @@ f first :: List.map addSpaceUnlessSeq remArgs
      | [] ->
         ""
  in

  let needsParens, str =
    match ast with
      | { Ast2.id; args = [] } ->
         `WrapNone, id
      | { Ast2.id } when id = macroJuxOp ->
         nested, combineJux ast.Ast2.args (astToSource indentLevel `WrapParen)
      | { Ast2.id; args = callee :: args } when id = macroCallOp ->
         `WrapNone, sprintf "%s(%s)"
                            (astToSource indentLevel `WrapParen callee)
                            (Common.combine ", " @@ List.map (astToSource indentLevel `WrapNone) args)
      | { Ast2.id; args = _ :: _ } when isSeq id ->
         let indentStr = "\n" ^ String.make (2 * indentLevel + 2) ' ' in
         `WrapNone,
         (Common.combine indentStr @@ ":" :: List.map (astToSource (indentLevel+1) `WrapNone) ast.Ast2.args)
         ^ sprintf "\n%send" (String.make (2 * indentLevel) ' ')

      (* This won't match expressions because they are parsed as (opcall postop* arg) *)
      | { Ast2.id; args = [arg] } when id =~ "postop\\([^a-zA-Z0-9_]*\\)" ->
         `WrapNone, astToSource indentLevel `WrapParen arg ^ Str.matched_group 1 id

      | _ ->
         nested, ast.Ast2.id ^ " " ^ combineJux ast.Ast2.args (astToSource indentLevel `WrapParen)
  in
  match needsParens with
    | `WrapNone -> str
    | `WrapParen -> sprintf "(%s)" str

