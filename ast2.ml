open Printf
  
type identifier = string

type expression = {
  id :identifier;
  args :expression list;
}

let idExpr name = { id = name; args = [] }
let simpleExpr name args = { id = name; args = List.map (fun str -> { id = str; args = [] }) args }
  
let rec expression2string = function
  | { id = ""; args = [] } -> "()"
  | { id = id; args = args; } ->
      begin
        let argStrings = List.map expression2string args in
        let id = if id == "" then "\"\"" else id in
        let inOneLine = Common.combine " " (id::argStrings) in
        if String.length inOneLine <= 60 then
          if String.contains inOneLine ' ' then
            "(" ^ inOneLine ^ ")"
          else
            inOneLine
        else
          Common.combine "\n  " (id::argStrings)
      end    


let rec replaceParams params args expr =
  let argCount = List.length args
  and paramCount = List.length params
  in
  if argCount <> paramCount then
    failwith (sprintf "Macro called with %d parameters, expected %d" paramCount argCount);
  let replacementList = List.combine params args in
  let replace name =
    try List.assoc name replacementList
    with Not_found -> { id = name; args = [] }
  in
  match replace expr.id with
    | { id = name; args = [] } -> { id = name; args = List.map (replaceParams params args) expr.args; }
    | _ as head -> { id = "seq"; args = head :: List.map (replaceParams params args) expr.args; }
