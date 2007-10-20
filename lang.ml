
TYPE_CONV_PATH "Lang"
  
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

type composedType = typ with sexp
type integralValue = value with sexp

let dequoteEscapeSequence str =
  let numRE = Str.regexp "^\\\\[0-9]\\([0-9][0-9]\\)?$"
  and specialStrings =
    [
      "\\n", '\n';
      "\\t", '\t';
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
    let length = String.length str in
    if length > 2 && str.[0] = quoteChar && str.[length-1] = quoteChar then
      String.sub str 1 (length-2)
    else
      raise (Failure (sprintf "dequoteString %c" quoteChar))
  in
  let dequoteChar str =
    let dequoted = dequoteString '\'' str in
    if String.length dequoted = 1 then
      dequoted.[0]
    else
      dequoteEscapeSequence dequoted
  in
  tryAll
    [
      lazy( IntVal (int_of_string str) );
      lazy( FloatVal (float_of_string str) );
      lazy( BoolVal (bool_of_string str) );
      lazy( StringLiteral (dequoteString '"' str) );
      lazy( CharVal (dequoteChar str) );
    ]
    ~onSuccess:some
    ~ifAllFailed:(lazy None)
    
type varStorage =
  | RegisterStorage
  | MemoryStorage
with sexp
    
type 'typ variable = {
  vname :string;
  typ :'typ;
  default :integralValue;
  vstorage :varStorage;
  vmutable :bool;
  vglobal :bool;
} with sexp

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
with sexp

(* TODO: make `Constant + integralValue polymorphic *)
type 'typ flatArgForm = [
| `Variable of 'typ variable
| `Constant of integralValue
] with sexp

type 'form genericIntrinsic = [
| `NullptrIntrinsic of composedType
| `MallocIntrinsic of composedType * 'form
| `GetAddrIntrinsic of composedType variable
| `StoreIntrinsic of 'form * 'form
| `LoadIntrinsic of 'form
| `PtrAddIntrinsic of 'form * 'form (* pointer, int *)
| `GetFieldPointerIntrinsic of 'form * string
] with sexp
    
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
(* | `ToplevelForm of toplevelExpr *)
]
and func = {
  fname :string;
  rettype :composedType;
  fargs :(string * composedType) list;
  impl :form option;
}
and toplevelExpr = [
| `GlobalVar of composedType variable
| `DefineFunc of func
| `Typedef of string * typ
]
with sexp
    
let toSingleForm formlist =
  match formlist with
    | [(singleForm :form)] -> singleForm
    | sequence -> `Sequence sequence


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
  mtransformFunc :Ast2.sexpr list -> Ast2.sexpr;
}
    
(* type package = { *)
(*   pname :string; *)
(*   vars :variable list; *)
(*   funcs :func list; *)
(* } *)
    


