
let rec combine seperator = function
    [] -> ""
  | [str] -> str
  | hd :: tl -> hd ^ seperator ^ (combine seperator tl)

type identifier = string

type expression = {
  id :identifier;
  args :expression list;
}

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
    
