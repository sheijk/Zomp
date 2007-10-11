open Printf
open Common
  
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

let string2integralValue str =
  let dequoteString quoteChar str =
    let length = String.length str in
    if length > 2 && str.[0] = quoteChar && str.[length-1] = quoteChar then
      String.sub str 1 (length-2)
    else
      raise (Failure (sprintf "dequoteString %c" quoteChar))
  in
  tryAll
    [
      lazy( IntVal (int_of_string str) );
      lazy( FloatVal (float_of_string str) );
      lazy( BoolVal (bool_of_string str) );
      lazy( StringLiteral (dequoteString '"' str) );
      lazy( CharVal (dequoteString '\'' str).[0] );
    ]
    ~onSuccess:some
    ~ifAllFailed:(lazy None)
    
(*     try Some ( IntVal (int_of_string str) ) *)
(*     with _ -> *)
(*       try Some ( FloatVal (float_of_string str) ) *)
(*       with _ -> *)
(*         try Some ( BoolVal (bool_of_string str) ) *)
(*         with _ -> *)
(*           try Some ( StringLiteral (dequoteString '"' str) ) *)
(*           with _ -> *)
(*             try Some ( CharVal (dequoteString '\'' str).[0] ) *)
(*             with _ -> *)
(*               None *)
    
type varStorage =
  | RegisterStorage
  | MemoryStorage

type 'typ variable = {
  vname :string;
  typ :'typ;
  default :integralValue;
  vstorage :varStorage;
  vmutable :bool;
  vglobal :bool;
}

let variable ~name ~typ ~default ~storage ~global = {
  vname = name;
  typ = typ;
  default = default;
  vstorage = storage;
  vmutable = false;
  vglobal = global;
}

(* TODO: fix this! TODO: find out what should be fixed... *)
let localVar = variable ~storage:RegisterStorage ~global:false
and globalVar = variable ~storage:MemoryStorage ~global:true

type 'argument funcCall = {
  fcname :string;
  fcrettype :composedType;
  fcparams :composedType list;
  fcargs :'argument list;
}
and label = {
  lname :string;
}
and branch = {
  bcondition :[`Bool] variable;
  trueLabel :label;
  falseLabel :label;
}

(* TODO: make `Constant + integralValue polymorphic *)
type 'typ flatArgExpr = [
| `Variable of 'typ variable
| `Constant of integralValue
]

type 'expr genericIntrinsic = [
| `NullptrIntrinsic of composedType
| `MallocIntrinsic of composedType * 'expr
| `GetAddrIntrinsic of composedType variable
| `StoreIntrinsic of 'expr * 'expr
| `LoadIntrinsic of 'expr
| `PtrAddIntrinsic of 'expr * 'expr (* pointer, int *)
| `GetFieldPointerIntrinsic of [`Pointer of [`Record of recordType]] variable * string
]
    
type expr = [
| composedType flatArgExpr
| `Sequence of expr list
| `DefineVariable of composedType variable * expr option
| `FuncCall of expr funcCall
| `AssignVar of composedType variable * expr
| `Return of expr
| `Jump of label
| `Branch of branch
| `Label of label
| expr genericIntrinsic
]

let toSingleForm formlist =
  match formlist with
    | [(singleForm :expr)] -> singleForm
    | sequence -> `Sequence sequence


type func = {
  fname :string;
  rettype :composedType;
  fargs :(string * composedType) list;
  impl :expr option;
}
and toplevelExpr =
  | GlobalVar of composedType variable
  | DefineFunc of func

let func name rettype args impl = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = impl;
}

let funcDecl name rettype args = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = None;
}
  
let funcDef name rettype args impl = {
  fname = name;
  rettype = rettype;
  fargs = args;
  impl = Some impl;
}

type macro = {
  mname :string;
  mtransformFunc :Ast2.expression list -> Ast2.expression;
}
    
(* type package = { *)
(*   pname :string; *)
(*   vars :variable list; *)
(*   funcs :func list; *)
(* } *)
    


type typecheckResult =
  | TypeOf of composedType
      (** error message, found type, expected type *)
  | TypeError of string * composedType * composedType
  
let rec typeCheck : expr -> typecheckResult =
  let pointerType form =
    match typeCheck form with
      | TypeOf (`Pointer _ as pointerType) -> TypeOf pointerType
      | TypeOf invalidType -> TypeError ("Expected pointer type", invalidType, `Pointer `Void)
      | _ as e -> e
  in
  let expectType form expectType =
    match typeCheck form with
      | TypeOf typ when typ = expectType -> TypeOf typ
      | TypeOf invalidType -> TypeError ("Invalid type in 2nd parameter", invalidType, `Int)
      | _ as e -> e
  in
  let (>>) l r =
    match l with
      | TypeError _ as e -> e
      | TypeOf _ -> r
  in
  function
    | `Sequence [] -> TypeOf `Void
    | `Sequence [expr] -> typeCheck expr
    | `Sequence (_ :: tail) -> typeCheck (`Sequence tail)
    | `DefineVariable (var, expr) -> begin
        match expr with
          | Some expr ->
              begin
                match typeCheck expr with
                  | TypeOf exprType when exprType = var.typ -> TypeOf `Void
                  | TypeOf wrongType -> TypeError (
                      "variable definition requires same type for var and default value", wrongType, var.typ)
                  | TypeError _ as typeError -> typeError
              end
          | None -> TypeOf `Void
      end
    | `Variable var -> TypeOf var.typ
    | `Constant value -> TypeOf (typeOf value)
    | `FuncCall call ->
        begin
          let paramCount = List.length call.fcparams
          and argCount = List.length call.fcargs in
          if paramCount != argCount then
            TypeError (sprintf "Expected %d params, but used with %d args" paramCount argCount, `Void, `Void)
          else
            List.fold_left2
              (fun prevResult typ arg ->
                 match typeCheck (arg :> expr) with
                   | TypeOf argType when typ = argType -> prevResult
                   | TypeOf invalidType -> TypeError ("Argument type does not match", invalidType, typ)
                   | TypeError(msg, invalidType, expectedType) ->
                       TypeError ("Argument type is invalid: " ^ msg, invalidType, expectedType)
              )
              (TypeOf (call.fcrettype :> composedType))
              call.fcparams call.fcargs
        end
    | `AssignVar (v, expr) -> begin
        match typeCheck expr with
          | TypeOf exprType when exprType = v.typ -> TypeOf `Void
          | TypeOf exprType -> TypeError (
              "Cannot assign result of expression to var because types differ",
              exprType, v.typ)
          | _ as typeError -> typeError
      end
    | `Return expr -> typeCheck expr
    | `Label _ -> TypeOf `Void
    | `Jump _ -> TypeOf `Void
    | `Branch _ -> TypeOf `Void
    | `NullptrIntrinsic typ -> TypeOf (`Pointer typ)
    | `MallocIntrinsic (typ, _) -> TypeOf (`Pointer typ)
    | `GetAddrIntrinsic var ->
        begin
          match var.vstorage with
            | MemoryStorage -> TypeOf (`Pointer var.typ)
            | RegisterStorage -> TypeError ("Cannot get address of variable with register storage", var.typ, var.typ)
        end
    | `StoreIntrinsic (ptrExpr, valueExpr) ->
        begin
          match typeCheck ptrExpr, typeCheck valueExpr with
            | TypeOf `Pointer ptrTargetType, TypeOf valueType when valueType = ptrTargetType -> TypeOf `Void
            | TypeOf (#typ as invalidPointerType), TypeOf valueType ->
                TypeError ("tried to store value to pointer of mismatching type",
                           invalidPointerType,
                           `Pointer valueType)
            | (_ as l), (_ as r) ->
                l >> r
        end
    | `LoadIntrinsic expr ->
        begin
          match typeCheck expr with
            | TypeOf `Pointer targetType -> TypeOf targetType
            | TypeOf invalid -> TypeError ("Expected pointer", invalid, `Pointer `Void) 
            | TypeError _ as t -> t
        end
    | `GetFieldPointerIntrinsic (recordVar, fieldName) ->
        begin
          let `Pointer `Record components = recordVar.typ in
          match componentType components fieldName with
            | Some t -> TypeOf (`Pointer t)
            | None -> TypeError("Component not found", `Void, `Void)
        end
    | `PtrAddIntrinsic (ptrExpr, offsetExpr) ->
        begin
          expectType offsetExpr `Int
          >> pointerType ptrExpr
        end
  
let rec typeCheckTL = function
  | GlobalVar var -> TypeOf var.typ
  | DefineFunc f ->
      match f.impl with
        | None -> TypeOf f.rettype
        | Some impl ->
            match typeCheck impl with
              | TypeOf implType when implType = f.rettype ->
                  TypeOf f.rettype
              | TypeOf wrongType ->
                  TypeError (
                    "Function's return type is not equal to it's implementation",
                    f.rettype,
                    wrongType)
              | TypeError _ as e ->
                  e
    
let typeOfForm ~onError form =
  match typeCheck form with
    | TypeOf typ -> typ
    | TypeError (msg, _, _) ->
        onError ~msg
          
