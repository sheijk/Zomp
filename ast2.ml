
let rec combine seperator = function
    [] -> ""
  | [str] -> str
  | hd :: tl -> hd ^ seperator ^ (combine seperator tl)

type identifier = string

(* type value = *)
(*   | Int of int *)
(*   | Float of float *)
(*   | String of string *)

(* type expression = *)
(*   | Expr of identifier * expression list *)
    
type expression = {
  id :identifier;
  args :expression list;
}

(* let value2string = function *)
(*   | Int i -> string_of_int i *)
(*   | Float f -> string_of_float f *)
(*   | String s -> "'" ^ s ^ "'" *)

let rec expression2string { id = id; args = args; } =
      let argStrings = List.map expression2string args in
      let inOneLine = combine " " (id::argStrings) in
      if String.length inOneLine <= 60 then
        if String.contains inOneLine ' ' then
          "(" ^ inOneLine ^ ")"
        else
          inOneLine
      else
        combine "\n  " (id::argStrings)
    
