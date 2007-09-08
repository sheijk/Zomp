
type integralType = [
  | `Void
  | `Int
  | `Float
  | `String
  | `Bool
]

exception UnknownType of string
  
let string2integralType = function
  | "int" -> `Int
  | "float" -> `Float
  | "string" -> `String
  | "bool" -> `Bool
  | "void" -> `Void
  | _ as name -> raise (UnknownType name)

let integralType2String = function
  | `Void -> "void"
  | `Int -> "int"
  | `Float -> "float"
  | `String -> "string"
  | `Bool -> "bool"

type recordType = (string * composedType) list
and composedType = [
  `Pointer of composedType
| `Record of recordType
| integralType
]

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

let rec composedType2String = function
  | `Pointer t -> (composedType2String t) ^ "*"
  | `Record components ->
      let rec convert (name, typ) =
        Printf.sprintf "(%s : %s)" name (composedType2String typ)
      in
      "(" ^ Common.combine " " (List.map convert components) ^ ")"
  | #integralType as t -> integralType2String t

let rec string2composedType str =
  let len = String.length str in
  if len >= 1 && str.[len-1] = '*' then
    `Pointer (string2composedType (Str.string_before str (len - 1)))
  else
    string2integralType str
  

type integralValue =
  | VoidVal
  | IntVal of int
  | FloatVal of float
  | StringVal of string
  | BoolVal of bool
  | PointerVal of composedType * int option
  | RecordVal of (string * integralValue) list

let rec integralValue2Type : integralValue -> composedType = function
  | VoidVal -> `Void
  | IntVal _ -> `Int
  | FloatVal _ -> `Float
  | StringVal _ -> `String
  | BoolVal _ -> `Bool
  | PointerVal (t, _) -> t
  | RecordVal components ->
      let convert (name, value) = name, integralValue2Type value in
      `Record (List.map convert components)

let hasType recordValue recordType =
  let typeOfValue = integralValue2Type recordValue in
  typeOfValue = recordType
      
let rec defaultValue : composedType -> integralValue = function
  | `Void -> VoidVal
  | `Int -> IntVal 0
  | `Float -> FloatVal 0.0
  | `String -> StringVal ""
  | `Bool -> BoolVal false
  | `Pointer t -> PointerVal (t, None)
  | `Record components ->
      let convert (name, typ) = name, defaultValue typ in
      RecordVal (List.map convert components)

let parseValue typ str =
  match typ with
    | `Void -> raise (Failure "no values of void allowed")
    | `Int -> IntVal (int_of_string str)
    | `Float -> FloatVal (float_of_string str)
    | `String ->
        let length = String.length str in
        let value = String.sub str 1 (length-2) in
        StringVal value
    | `Bool -> BoolVal (bool_of_string str)
(*     | `Pointer t -> *)
(*         let target = *)
(*           if str = "null" then None *)
(*           else Some (int_of_string str) *)
(*         in *)
(*         PointerVal (t, target) *)
        
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

let rec integralValue2String = function
  | VoidVal -> raise (Failure "no values of void allowed")
  | IntVal i -> string_of_int i
  | FloatVal f -> string_of_float f
  | StringVal s -> "\"" ^ s ^ "\""
  | BoolVal b -> string_of_bool b
  | PointerVal (typ, target) ->
      begin
        match target with
          | Some addr -> "0x" ^ string_of_int addr
          | None -> "null"
      end
  | RecordVal components ->
      let rec convert = function
        | [] -> ""
        | (name, value) :: tail ->
            (Printf.sprintf "(%s = %s)" name (integralValue2String value)) ^ (convert tail)
      in
      "(" ^ convert components ^ ")"

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

(* TODO: fix this! *)
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
  | IfThenElse of ifthenelse
  | Loop of loop
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
    
