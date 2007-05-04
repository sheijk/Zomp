
type integralType =
  | Int
  | Float
  | String
  | Bool

exception UnknownType of string
  
let string2integralType = function
  | "int" -> Int
  | "float" -> Float
  | "string" -> String
  | "bool" -> Bool
  | _ as name -> raise (UnknownType name)

let integralType2String = function
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
      
type integralValue =
  | IntVal of int
  | FloatVal of float
  | StringVal of string
  | BoolVal of bool

let defaultValue = function
  | Int -> IntVal 0
  | Float -> FloatVal 0.0
  | String -> StringVal ""
  | Bool -> BoolVal false
  
let parseValue typ str =
  match typ with
    | Int -> IntVal (int_of_string str)
    | Float -> FloatVal (float_of_string str)
    | String -> StringVal str
    | Bool -> BoolVal (bool_of_string str)

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

let integralValue2String = function
  | IntVal i -> string_of_int i
  | FloatVal f -> string_of_float f
  | StringVal s -> "\"" ^ s ^ "\""
  | BoolVal b -> string_of_bool b
      
type variable = {
  vname :string;
  typ :integralType;
  default :integralValue;
}

let variable ~name ~typ ~default = {
  vname = name;
  typ = typ;
  default = default;
}  

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
  fcargs :expr list;
}
and expr =
  | Sequence of expr list
  | DefineVariable of variable
  | Variable of string
  | Constant of integralValue
  | FuncCall of funcCall
  | IfThenElse of ifthenelse
  | Loop of loop

type func = {
  fname :string;
  rettype :integralType;
  args :(string * integralType) list;
  impl :expr
}
and toplevelExpr =
  | GlobalVar of variable
  | DefineFunc of func

let func name rettype args impl = {
  fname = name;
  rettype = rettype;
  args = args;
  impl = impl;
}
  
(* type package = { *)
(*   pname :string; *)
(*   vars :variable list; *)
(*   funcs :func list; *)
(* } *)
    
