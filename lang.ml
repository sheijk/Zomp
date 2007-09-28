
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
        try Some ( StringVal (dequoteString str) )
        with _ ->
          None
    
type varStorage =
  | RegisterStorage
  | MemoryStorage

type variable = {
  vname :string;
  typ :composedType;
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

type ifthenelse = {
  cond :expr;
  trueCode :expr;
  falseCode :expr;
}
and loop = {
  preCode :expr;
  abortTest :expr;
  postCode :expr;
}
and funcCall = {
  fcname :string;
  fcrettype :composedType;
  fcparams :composedType list;
  fcargs :expr list;
}
and label = {
  lname :string;
}
and branch = {
  bcondition :variable;
  trueLabel :label;
  falseLabel :label;
}
and genericIntrinsic =
  | NullptrIntrinsic of composedType
  | MallocIntrinsic of composedType
  | DerefIntrinsic of variable
  | StoreIntrinsic of variable * variable (* value, ptr *)
  | SetFieldIntrinsic of composedType * variable * string * expr
  | GetFieldIntrinsic of composedType * variable * string
and expr =
  | Sequence of expr list
  | DefineVariable of variable * expr option
  | Variable of variable
  | Constant of integralValue
  | FuncCall of funcCall
  | AssignVar of variable * expr
  | IfThenElse of ifthenelse (*remove*)
  | Loop of loop (*remove*)
  | Return of expr
  | Jump of label
  | Branch of branch
  | Label of label
  | GenericIntrinsic of genericIntrinsic

type func = {
  fname :string;
  rettype :composedType;
  fargs :(string * composedType) list;
  impl :expr option;
}
and toplevelExpr =
  | GlobalVar of variable
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
    
