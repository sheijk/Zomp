
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

