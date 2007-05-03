
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
        
(* let string2integralValue str = *)
(*   try Int (int_of_string str) *)
(*   with _ -> begin *)
(*     try Float (float_of_string str) *)
(*     with _ -> begin *)
(*     end *)
      
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
    
