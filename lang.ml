
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
      
type integralValue =
  | IntVal of int
  | FloatVal of float
  | StringVal of string
  | BoolVal of bool

let parseValue typ str =
  match typ with
    | Int -> IntVal (int_of_string str)
    | Float -> FloatVal (float_of_string str)
    | String -> StringVal str
    | Bool -> BoolVal (bool_of_string str)
        
(* let string2integralValue str = *)
(*   try Int (int_of_string str) *)
(*   with _ -> begin *)
(*     try Float (float_of_string str) *)
(*     with _ -> begin *)
(*     end *)
      
type variable = {
  name :string;
  typ :integralType;
  default :integralValue;
}

let variable ~name ~typ ~default = {
  name = name;
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
  name :string;
  args :expr list;
}
and expr =
  | Sequence of expr list
  | DefineVariable of variable
  | FuncCall of funcCall
  | IfThenElse of ifthenelse
  | Loop of loop
  | Variable of string

type func = {
  name :string;
  rettype :integralType;
  args :(string * integralType) list;
  impl :expr
}
and toplevelExpr =
  | GlobalVar of variable
  | DefineFunc of func

let func name rettype args impl = {
  name = name;
  rettype = rettype;
  args = args;
  impl = impl;
}
  
type package = {
  name :string;
  vars :variable list;
  funcs :func list;
}
    
