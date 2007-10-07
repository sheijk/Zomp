
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
  let dequoteString str =
    let length = String.length str in
    if length > 2 && str.[0] = '"' && str.[length-1] = '"' then
      String.sub str 1 (length-2)
    else
      raise (Failure "dequoteString")
  in
  try Some ( IntVal (int_of_string str) )
  with _ ->
    try Some ( FloatVal (float_of_string str) )
    with _ ->
      try Some ( BoolVal (bool_of_string str) )
      with _ ->
        try Some ( StringLiteral (dequoteString str) )
        with _ ->
          None
    
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
    
type genericIntrinsic =
  | NullptrIntrinsic of composedType
  | MallocIntrinsic of composedType
  | DerefIntrinsic of [`Pointer of typ] variable
  | GetAddrIntrinsic of composedType variable
  | StoreIntrinsic of composedType variable * [`Pointer of composedType] variable (* value, ptr *)
  | LoadIntrinsic of composedType variable
  | GetFieldPointerIntrinsic of [`Pointer of [`Record of recordType]] variable * string

type flatArgExpr = [
| `Variable of composedType variable
| `Constant of integralValue
]
(* type 'arg funcCallExpr = [ *)
(* | `FuncCall of 'arg funcCall *)
(* ] *)

(* type rightHandExpr = [ *)
(* | flatArgExpr *)
(* | flatArgExpr funcCallExpr *)
(* ] *)
    
type expr = [
| flatArgExpr
| `Sequence of expr list
| `DefineVariable of composedType variable * expr option
| `FuncCall of expr funcCall
| `AssignVar of composedType variable * expr
| `Return of expr
| `Jump of label
| `Branch of branch
| `Label of label
| `GenericIntrinsic of genericIntrinsic
]

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
    
