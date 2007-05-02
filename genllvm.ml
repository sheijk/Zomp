
open Ast2

let translateTL = function
  | Expr ( "std_global", [Expr(typ, []); Expr(name, []); Expr(value, [])] ) ->
      "%" ^ name ^ " = [" ^ typ ^ "] " ^ value ^ ";"
  | _ as e ->
      "; " ^ expression2string e
    
