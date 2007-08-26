(* File lexer.mll *)
{
  open Parser2
  exception Eof
}

let whitespace = ['\n' ' ' '\t']*
let operatorSymbol = ['+' '-' '*' '/']
let operatorSuffix = ('_' ['a'-'z']+)?
let identifierChar = ['a'-'z' '0'-'9' 'A'-'Z' '_' '.']
  
rule token = parse
  | whitespace
      { token lexbuf }
  | ';'
      { SEPERATOR }
  | '{' | '('
      { BLOCK_BEGIN }
  | '}' | ')'
      { BLOCK_END }
  | '+' operatorSuffix as name
      { OP_PLUS(name) }
  | ("op" operatorSymbol) as name
      { IDENTIFIER(name) }
  | (('"' [^'\"']* '"') as str)
      { IDENTIFIER(str) }
  | (identifierChar+ as id) | (('-' identifierChar+) as id)
      { IDENTIFIER(id) }
  | "//" [^'\n']* '\n'
      { token lexbuf }
  |  "/*"
      { mlcomment lexbuf }
  | '!'
      { raise Eof }
  | eof
      { raise Eof }
and mlcomment = shortest 
  | (_)* "*/"
      { token lexbuf }

