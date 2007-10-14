{
  open Sexprparser
  exception Eof
  exception UnknowChar of string
}

let whitespace = [' ' '\t' '\n']*

let identifierChar = [':' 'a'-'z' '0'-'9' 'A'-'Z' '_' '.']

rule token = parse
  | whitespace
      { token lexbuf }
  | '('
      { PAREN_OPEN }
  | ')'
      { PAREN_CLOSE }
  | (identifierChar+ as id) | (('-' identifierChar+) as id)
      { IDENTIFIER(id) }
  | eof
      { raise Eof }
  | (('"' [^'\"']* '"') as str)
      { IDENTIFIER(str) }
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
    
