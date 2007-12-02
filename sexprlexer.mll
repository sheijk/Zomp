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
let opPostfix = ('_' identifierChar+)?
  
rule token = parse
  | whitespace
      { token lexbuf }
  | '('
      { PAREN_OPEN }
  | ')'
      { PAREN_CLOSE }
  | ("op" (addOp | multOp | compareOp) opPostfix) as funcName
      { IDENTIFIER(funcName) }
  | (addOp opPostfix) as op
      { ADD_OP(op) }
  | (multOp opPostfix) as op
      { MULT_OP(op) }
  | (compareOp opPostfix) as op
      { COMPARE_OP(op) }
  | (("``" | "`" | "#" ) as quoteOp)
      { QUOTE(quoteOp) }
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
      
