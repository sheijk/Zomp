
open Ast2

let translateTL = function
  | { id = "std_global"; args = [
        { id = typ; args = []; };
        { id = name; args = []; };
        { id = value; args = []; };
      ] } ->
      "%" ^ name ^ " = [" ^ typ ^ "] " ^ value ^ ";"
  | _ as e ->
      "; " ^ expression2string e
    
