
let rec combine seperator = function
    [] -> ""
  | [str] -> str
  | hd :: tl -> hd ^ seperator ^ (combine seperator tl)

type identifier = string

(* type value = *)
(*   | Int of int *)
(*   | Float of float *)
(*   | String of string *)

type expression =
  | Expr of identifier * expression list

(* let value2string = function *)
(*   | Int i -> string_of_int i *)
(*   | Float f -> string_of_float f *)
(*   | String s -> "'" ^ s ^ "'" *)

let rec expression2string = function
(*   | Constant v -> value2string v *)
  | Expr (id, args) ->
      let argStrings = List.map expression2string args in
      let inOneLine = combine " " (id::argStrings) in
      if String.length inOneLine <= 60 then
        inOneLine
      else
        combine "\n  " (id::argStrings)
    
