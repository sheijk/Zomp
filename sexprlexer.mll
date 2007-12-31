{
  open Sexprparser
  exception Eof
  exception AbortInput
  exception UnknowChar of string
}

let whitespace = [' ' '\t' '\n']*

let identifierChar = [':' 'a'-'z' '0'-'9' 'A'-'Z' '_' '.']
let addOp = ['+' '-']
let multOp = ['*' '/']
let compareOp = ("=" | "==" | "!=" | "<" | "<=" | ">" | ">=")
let postOp = "..."
let opPostfix = ('_' identifierChar+)?
  
rule token = parse
  | whitespace
      { token lexbuf }
  | "op[]" as funcName
      { IDENTIFIER(funcName) }
  | ("op" (addOp | multOp | compareOp | postOp) opPostfix) as funcName
      { IDENTIFIER(funcName) }
  | (addOp opPostfix) as op
      { ADD_OP(op) }
  | (multOp opPostfix) as op
      { MULT_OP(op) }
  | (compareOp opPostfix) as op
      { COMPARE_OP(op) }
  | (postOp opPostfix) as op
      { POST_OP(op) }
  | (("``" | "`" | "#" ) as quoteOp)
      { QUOTE(quoteOp) }
  | '('
      { PAREN_OPEN }
  | ')'
      { PAREN_CLOSE }
  | '['
      { BRACKET_OPEN }
  | ']'
      { BRACKET_CLOSE }
  | (identifierChar+ as id) | (('-' identifierChar+) as id)
      { IDENTIFIER(id) }
  | eof
      { raise Eof }
  | "!!!"
      { raise AbortInput }
(*   | (('"' [^'\"']* '"') as str) *)
(*       { IDENTIFIER(str) } *)
  | '"'
      { IDENTIFIER (mlstring "\"" lexbuf) }
  | (('\'' [^'\'']* '\'') as chr)
      { IDENTIFIER(chr) }
  | "//" [^'\n']* '\n'
      { token lexbuf }
  |  "/*"
      { mlcomment lexbuf }
  | _ as c
      { raise (UnknowChar (Printf.sprintf "%c" c)) }
and mlcomment = shortest
  | (_)* "*/"
      { token lexbuf }
  | (_)* '\n'
      { mlcomment lexbuf }
  | eof
      { raise Eof }
and mlstring prevString = shortest
  | ((_)* '"') as str
      { prevString ^ str }
  | ((_)* '\n') as str
      { mlstring (prevString ^ str) lexbuf }
  | eof
      { raise Eof }
      
